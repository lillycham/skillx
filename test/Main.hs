module Main (main) where

import Data.ByteString qualified as BS
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Skillx.CLI (Command (CmdBuild, CmdCheck, CmdFmt, CmdGraph), parseArgs)
import Skillx.Check
  ( Finding (findingKind, findingSeverity, findingSubject),
    FindingKind
      ( CycleMember,
        DanglingLink,
        DuplicateTierRoot,
        MissingBin,
        MissingMcp,
        MissingRoot,
        Unreachable
      ),
    Severity (Problem, Suggestion),
    check,
    hasProblems,
  )
import Skillx.Compile
  ( FileTree (FileTree),
    Harness (Antigravity, ClaudeCode, OpenCode),
    SkillPackage (SkillPackage, spFiles, spSkill),
    Tier (TierAlways, TierEntry, TierLeaf),
    assignTiers,
    compile,
  )
import Skillx.Graph (Dir (In, Out))
import Skillx.Graph qualified as Graph
import Skillx.Manifest
  ( Manifest (manAlways, manEntry, manName, manSources, manVersion),
    ManifestError (BadSourceSpec),
    Place (Place, placeDir, placeHome),
    PlaceRules (PlaceRules, prAlways, prEntry),
    SourceSpec (GitSource, LocalSource, gsRev, gsSubdir, gsUrl),
    allRoots,
    matchingPlaceRules,
    parseManifest,
  )
import Skillx.Name (Name, NameError (BadName, EmptyName, NameTooLong), mkName, nameText)
import Skillx.Skill
  ( Skill (skillLinks, skillName),
    SkillParseError (MissingFrontmatter, NotCanonical, UnknownHeader),
    mkSkill,
    parseSkill,
    parseSkillLenient,
    renderSkill,
  )
import Skillx.Store
  ( StoreError (DuplicateSkillName),
    loadMultiStore,
    loadStore,
    writeFileTree,
  )
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase, (@?=))
import Test.Tasty.QuickCheck
  ( Arbitrary (arbitrary),
    Gen,
    chooseInt,
    elements,
    listOf,
    listOf1,
    testProperty,
  )

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "skillx"
    [ nameTests,
      graphTests,
      skillTests,
      manifestTests,
      checkTests,
      compileTests,
      storeTests,
      cliTests
    ]

-- Helpers ----------------------------------------------------------------

mustRight :: (Show e) => Either e a -> a
mustRight = either (error . show) id

unsafeName :: Text -> Name
unsafeName = mustRight . mkName

unsafeSkill ::
  Text ->
  Text ->
  [Text] ->
  [Text] ->
  [Text] ->
  Text ->
  Skill
unsafeSkill name desc links mcp bins body =
  case mkSkill
    (unsafeName name)
    desc
    (Set.fromList (map unsafeName links))
    (Set.fromList (map unsafeName mcp))
    (Set.fromList (map unsafeName bins))
    body of
    Right s -> s
    Left err -> error ("unsafeSkill failed: " <> show err)

-- Name tests -------------------------------------------------------------

nameTests :: TestTree
nameTests =
  testGroup
    "Name"
    [ testCase "accepts valid lowercase skill names" $ do
        let n = unsafeName "rust-dev_01.core"
        nameText n @?= "rust-dev_01.core",
      testCase "rejects empty name" $
        mkName "" @?= Left EmptyName,
      testCase "rejects names over 64 chars" $ do
        let long = T.replicate 65 "a"
        mkName long @?= Left (NameTooLong long),
      testCase "rejects leading dot or hyphen and uppercase" $ do
        mkName ".hidden" @?= Left (BadName ".hidden")
        mkName "-flag" @?= Left (BadName "-flag")
        mkName "RustDev" @?= Left (BadName "RustDev")
        mkName "a/b" @?= Left (BadName "a/b")
    ]

-- Graph tests ------------------------------------------------------------

graphTests :: TestTree
graphTests =
  testGroup
    "Graph"
    [ testCase "singleton, successors, predecessors and neighbours" $ do
        let a = unsafeName "a"
            b = unsafeName "b"
            c = unsafeName "c"
            g =
              Graph.singleton a (Set.fromList [b, c])
                <> Graph.singleton b (Set.singleton c)
                <> Graph.singleton c Set.empty
        Graph.nodes g @?= Set.fromList [a, b, c]
        Graph.successors a g @?= Set.fromList [b, c]
        Graph.predecessors c g @?= Set.fromList [a, b]
        Graph.neighbours b g @?= Set.fromList [(In, a), (Out, c)],
      testCase "reachable terminates on cycles and dangling detects missing targets" $ do
        let a = unsafeName "a"
            b = unsafeName "b"
            ghost = unsafeName "ghost"
            g =
              Graph.singleton a (Set.fromList [b, ghost])
                <> Graph.singleton b (Set.singleton a)
        Graph.reachable (Set.singleton a) g @?= Set.fromList [a, b, ghost]
        Graph.dangling g @?= Set.singleton ghost
        Graph.cycles g @?= Set.fromList [a, b]
    ]

-- Skill tests ------------------------------------------------------------

newtype ArbSkill = ArbSkill Skill
  deriving (Eq, Show)

genName :: Gen Name
genName = do
  firstChar <- elements (['a' .. 'z'] ++ ['0' .. '9'])
  len <- chooseInt (0, 12)
  rest <-
    mapM
      (const (elements (['a' .. 'z'] ++ ['0' .. '9'] ++ ['-', '_'])))
      [1 .. len]
  pure (unsafeName (T.pack (firstChar : rest)))

genDesc :: Gen Text
genDesc = do
  wordsList <-
    listOf1 $ do
      len <- chooseInt (1, 8)
      T.pack <$> mapM (const (elements ['a' .. 'z'])) [1 .. len]
  pure (T.intercalate " " wordsList)

instance Arbitrary ArbSkill where
  arbitrary = do
    n <- genName
    d <- genDesc
    ls <- Set.fromList <$> listOf genName
    mcps <- Set.fromList <$> listOf genName
    bins <- Set.fromList <$> listOf genName
    bodyLines <- listOf genDesc
    let body = T.unlines bodyLines
    case mkSkill n d ls mcps bins body of
      Right s -> pure (ArbSkill s)
      Left err -> error (show err)

skillTests :: TestTree
skillTests =
  testGroup
    "Skill"
    [ testProperty "parseSkill . renderSkill == Right" $ \(ArbSkill s) ->
        parseSkill (renderSkill s) == Right s,
      testCase "strict parseSkill rejects non-canonical header order, lenient accepts" $ do
        let raw =
              encodeUtf8 $
                T.unlines
                  [ "---",
                    "description: Rust build helper.",
                    "name: rust-cargo",
                    "links: z-skill, a-skill",
                    "---",
                    "",
                    "Run cargo test."
                  ]
        parseSkill raw @?= Left NotCanonical
        case parseSkillLenient raw of
          Left err -> assertBool ("unexpected error: " <> show err) False
          Right s -> do
            parseSkill (renderSkill s) @?= Right s
            skillLinks s @?= Set.fromList [unsafeName "a-skill", unsafeName "z-skill"],
      testCase "rejects missing frontmatter and unknown headers" $ do
        parseSkill "no frontmatter\n" @?= Left MissingFrontmatter
        let badHdr =
              encodeUtf8 $
                T.unlines
                  [ "---",
                    "name: rust-cargo",
                    "description: Valid description.",
                    "bogus: val",
                    "---",
                    "",
                    "Body."
                  ]
        parseSkill badHdr @?= Left (UnknownHeader "bogus")
    ]

-- Manifest tests ---------------------------------------------------------

manifestTests :: TestTree
manifestTests =
  testGroup
    "Manifest"
    [ testCase "parses full manifest and resolves place rules" $ do
        let toml =
              T.unlines
                [ "name = \"dev-bundle\"",
                  "version = \"0.2.0\"",
                  "always = [\"core-rules\"]",
                  "entry = [\"rust-dev\"]",
                  "available-mcp = [\"akyuu\"]",
                  "available-bins = [\"cargo\", \"git\"]",
                  "",
                  "[places.\"~/devel/garden\"]",
                  "entry = [\"garden-deploy\"]",
                  "",
                  "[sources.local]",
                  "path = \"./skills\"",
                  "",
                  "[sources.remote]",
                  "git = \"https://example.com/skills.git\"",
                  "rev = \"deadbeef\"",
                  "subdir = \"contrib/skills\""
                ]
        case parseManifest toml of
          Left err -> assertBool ("unexpected error: " <> show err) False
          Right m -> do
            manName m @?= "dev-bundle"
            manVersion m @?= "0.2.0"
            manAlways m @?= [unsafeName "core-rules"]
            manEntry m @?= [unsafeName "rust-dev"]
            allRoots m
              @?= Set.fromList
                [ unsafeName "core-rules",
                  unsafeName "rust-dev",
                  unsafeName "garden-deploy"
                ]
            let place = Place {placeHome = "/Users/alice", placeDir = "/Users/alice/devel/garden/sub"}
                tildePlace = Place {placeHome = "/Users/alice", placeDir = "~/devel/garden/sub"}
            matchingPlaceRules place m
              @?= PlaceRules
                { prAlways = [],
                  prEntry = [unsafeName "garden-deploy"]
                }
            matchingPlaceRules tildePlace m
              @?= PlaceRules
                { prAlways = [],
                  prEntry = [unsafeName "garden-deploy"]
                }
            Map.lookup "local" (manSources m) @?= Just (LocalSource "./skills")
            Map.lookup "remote" (manSources m)
              @?= Just
                GitSource
                  { gsUrl = "https://example.com/skills.git",
                    gsRev = Just "deadbeef",
                    gsSubdir = Just "contrib/skills"
                  },
      testCase "rejects source with both path and git" $ do
        let bad =
              T.unlines
                [ "[sources.bad]",
                  "path = \"./a\"",
                  "git = \"https://example.com/a.git\""
                ]
        parseManifest bad @?= Left (BadSourceSpec "bad")
    ]

-- Check tests ------------------------------------------------------------

checkTests :: TestTree
checkTests =
  testGroup
    "Check"
    [ testCase "reports missing roots, dangling links, missing capabilities, cycles and unreachable skills" $ do
        let sDev =
              unsafeSkill
                "rust-dev"
                "Entry point for Rust work."
                ["rust-cargo", "missing-leaf"]
                ["akyuu"]
                ["cargo"]
                "Use rust-cargo for builds."
            sCargo =
              unsafeSkill
                "rust-cargo"
                "Cargo build helper."
                ["rust-dev"]
                []
                ["missing-bin"]
                "Run cargo check."
            sOrphan =
              unsafeSkill
                "orphan-skill"
                "Never linked."
                []
                []
                []
                "Orphan body."
            store =
              Map.fromList
                [ (skillName sDev, sDev),
                  (skillName sCargo, sCargo),
                  (skillName sOrphan, sOrphan)
                ]
            manifest =
              mustRight $
                parseManifest $
                  T.unlines
                    [ "always = [\"rust-dev\"]",
                      "entry = [\"rust-dev\", \"ghost-root\"]",
                      "available-mcp = []",
                      "available-bins = [\"cargo\"]"
                    ]
            findings = check store manifest
            kinds = map (\f -> (findingSeverity f, findingKind f, findingSubject f)) findings
        assertBool "has problems" (hasProblems findings)
        assertEqual
          "expected finding list"
          [ (Problem, MissingRoot, unsafeName "ghost-root"),
            (Problem, DanglingLink, unsafeName "rust-dev"),
            (Problem, MissingMcp, unsafeName "rust-dev"),
            (Problem, MissingBin, unsafeName "rust-cargo"),
            (Suggestion, CycleMember, unsafeName "rust-cargo"),
            (Suggestion, CycleMember, unsafeName "rust-dev"),
            (Suggestion, Unreachable, unsafeName "orphan-skill"),
            (Suggestion, DuplicateTierRoot, unsafeName "rust-dev")
          ]
          kinds
    ]

-- Compile tests ----------------------------------------------------------

sampleStore :: Map Name SkillPackage
sampleStore =
  let sRules =
        unsafeSkill
          "core-rules"
          "Core workflow rules."
          []
          []
          []
          "Always use declarative builds."
      sHub =
        unsafeSkill
          "rust-dev"
          "Rust conventions and entry point."
          ["rust-cargo", "rust-debug"]
          ["akyuu"]
          ["cargo"]
          "Follow standard Rust conventions."
      sCargo =
        unsafeSkill
          "rust-cargo"
          "Run Cargo builds and tests."
          ["rust-debug"]
          []
          ["cargo"]
          "Use cargo test."
      sDebug =
        unsafeSkill
          "rust-debug"
          "Diagnose failing Rust builds."
          []
          []
          []
          "Inspect compiler errors."
      sGarden =
        unsafeSkill
          "garden-deploy"
          "Deploy the garden app."
          []
          []
          []
          "Run deploy script."
   in Map.fromList
        [ (skillName sRules, SkillPackage sRules Map.empty),
          (skillName sHub, SkillPackage sHub Map.empty),
          ( skillName sCargo,
            SkillPackage
              sCargo
              (Map.singleton "scripts/run.sh" "#!/bin/sh\ncargo test\n")
          ),
          (skillName sDebug, SkillPackage sDebug Map.empty),
          (skillName sGarden, SkillPackage sGarden Map.empty)
        ]

sampleManifest :: Manifest
sampleManifest =
  case parseManifest $
    T.unlines
      [ "name = \"test-bundle\"",
        "version = \"1.0.0\"",
        "always = [\"core-rules\"]",
        "entry = [\"rust-dev\"]",
        "available-mcp = [\"akyuu\"]",
        "available-bins = [\"cargo\"]",
        "",
        "[places.\"~/devel/garden\"]",
        "entry = [\"garden-deploy\"]"
      ] of
    Right m -> m
    Left err -> error (show err)

compileTests :: TestTree
compileTests =
  testGroup
    "Compile"
    [ testCase "assigns TierAlways, TierEntry and TierLeaf by reachability" $ do
        let skills = Map.map spSkill sampleStore
            tiers = assignTiers skills sampleManifest Nothing
        tiers
          @?= Map.fromList
            [ (unsafeName "core-rules", TierAlways),
              (unsafeName "rust-dev", TierEntry),
              (unsafeName "rust-cargo", TierLeaf),
              (unsafeName "rust-debug", TierLeaf)
            ]
        let place = Place {placeHome = "/home/u", placeDir = "/home/u/devel/garden"}
            tiersAtPlace = assignTiers skills sampleManifest (Just place)
        Map.lookup (unsafeName "garden-deploy") tiersAtPlace @?= Just TierEntry,
      testCase "compiles ClaudeCode and Antigravity plugin trees with relocatable routing footers" $ do
        let FileTree claudeFiles =
              mustRight (compile sampleStore sampleManifest ClaudeCode Nothing)
            FileTree agyFiles =
              mustRight (compile sampleStore sampleManifest Antigravity Nothing)
        assertBool "Claude has .claude-plugin/plugin.json" $
          Map.member ".claude-plugin/plugin.json" claudeFiles
        assertBool "Antigravity has plugin.json" $
          Map.member "plugin.json" agyFiles
        assertBool "Always rule emitted in rules/core-rules.md" $
          Map.member "rules/core-rules.md" claudeFiles
        assertBool "Leaf auxiliary file copied to _nodes/rust-cargo/scripts/run.sh" $
          Map.member "_nodes/rust-cargo/scripts/run.sh" claudeFiles

        let hubBytes = fromMaybe "" (Map.lookup "skills/rust-dev/SKILL.md" claudeFiles)
            hubText = decodeUtf8 hubBytes
        assertBool "skillx headers stripped from compiled SKILL.md" $
          not ("requires-mcp:" `T.isInfixOf` hubText)
            && not ("links:" `T.isInfixOf` hubText)
        assertBool "routing footer links to Tier 2 leaves" $
          "- [rust-cargo](../../_nodes/rust-cargo/SKILL.md): Run Cargo builds and tests."
            `T.isInfixOf` hubText
            && "- [rust-debug](../../_nodes/rust-debug/SKILL.md): Diagnose failing Rust builds."
              `T.isInfixOf` hubText,
      testCase "compiles OpenCode and Codex trees with combined AGENTS.md" $ do
        let FileTree ocFiles =
              mustRight (compile sampleStore sampleManifest OpenCode Nothing)
        Map.lookup "AGENTS.md" ocFiles
          @?= Just "Always use declarative builds.\n"
        assertBool "entry skill present" $
          Map.member "skills/rust-dev/SKILL.md" ocFiles
    ]

-- Store tests ------------------------------------------------------------

storeTests :: TestTree
storeTests =
  testGroup
    "Store"
    [ testCase "writes and reloads canonical skill directories with auxiliary files" $
        withSystemTempDirectory "skillx-test" $ \tmp -> do
          let storeDir = tmp </> "skills"
              outDir = tmp </> "out"
          mapM_
            ( \(name, pkg) -> do
                let dir = storeDir </> T.unpack (nameText name)
                createDirectoryIfMissing True dir
                BS.writeFile (dir </> "SKILL.md") (renderSkill (spSkill pkg))
                mapM_
                  ( \(rel, bs) -> do
                      let full = dir </> rel
                      createDirectoryIfMissing True (storeDir </> T.unpack (nameText name) </> "scripts")
                      BS.writeFile full bs
                  )
                  (Map.toAscList (spFiles pkg))
            )
            (Map.toAscList sampleStore)

          eLoaded <- loadStore True storeDir
          eLoaded @?= Right sampleStore

          eDup <- loadMultiStore True [storeDir, storeDir]
          case eDup of
            Left (DuplicateSkillName _ _ _) -> pure ()
            other -> assertBool ("expected DuplicateSkillName, got: " <> show other) False

          let tree = mustRight (compile sampleStore sampleManifest Antigravity Nothing)
          writeFileTree outDir tree
          hasPluginJson <- doesFileExist (outDir </> "plugin.json")
          assertBool "plugin.json written to disk" hasPluginJson
    ]

-- CLI tests --------------------------------------------------------------

cliTests :: TestTree
cliTests =
  testGroup
    "CLI"
    [ testCase "parses check, build, graph and fmt subcommands" $ do
        case parseArgs ["check", "-m", "my.toml", "--lenient"] of
          Just (CmdCheck _) -> pure ()
          other -> assertBool ("unexpected parse result: " <> show other) False
        case parseArgs ["build", "-t", "antigravity", "-o", "dist", "--at", "~/devel/garden"] of
          Just (CmdBuild _) -> pure ()
          other -> assertBool ("unexpected parse result: " <> show other) False
        case parseArgs ["graph"] of
          Just (CmdGraph _) -> pure ()
          other -> assertBool ("unexpected parse result: " <> show other) False
        case parseArgs ["fmt", "skills"] of
          Just (CmdFmt _) -> pure ()
          other -> assertBool ("unexpected parse result: " <> show other) False
    ]
