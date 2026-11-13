{-# LANGUAGE LambdaCase #-}

-- | Command-line interface for @skillx@.
module Skillx.CLI
  ( Command (..),
    CheckOpts (..),
    BuildOpts (..),
    GraphOpts (..),
    FmtOpts (..),
    parseArgs,
    runCLI,
  )
where

import Data.ByteString qualified as BS
import Data.List (isPrefixOf, sort)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8')
import Data.Text.IO qualified as TIO
import Options.Applicative
  ( Parser,
    ParserInfo,
    ParserPrefs,
    columns,
    command,
    customExecParser,
    fullDesc,
    header,
    help,
    helper,
    hsubparser,
    info,
    long,
    metavar,
    option,
    optional,
    prefs,
    progDesc,
    short,
    strArgument,
    strOption,
    switch,
    value,
    (<**>),
  )
import Options.Applicative qualified as Opt
import Skillx.Check
  ( Finding (findingDetail, findingKind, findingSeverity),
    check,
    hasProblems,
    kindText,
    severityText,
  )
import Skillx.Compile
  ( CompileError (VerificationFailed),
    FileTree (unFileTree),
    Harness,
    SkillPackage (spSkill),
    Tier (TierAlways, TierEntry, TierLeaf),
    assignTiers,
    compile,
    harnessText,
    parseHarness,
  )
import Skillx.Graph qualified as Graph
import Skillx.Manifest
  ( Manifest (manSources),
    Place (Place, placeDir, placeHome),
    SourceSpec (LocalSource),
    parseManifest,
  )
import Skillx.Name (Name, nameText)
import Skillx.Skill (parseSkillLenient, renderSkill, skillGraph)
import Skillx.Store
  ( describeStoreError,
    loadMultiStore,
    writeFileTree,
  )
import System.Directory
  ( XdgDirectory (XdgConfig),
    doesFileExist,
    getHomeDirectory,
    getXdgDirectory,
    listDirectory,
  )
import System.Exit (ExitCode (ExitFailure, ExitSuccess), exitWith)
import System.FilePath (takeDirectory, (</>))
import System.IO (stderr)

-- | Top-level CLI commands.
data Command
  = CmdCheck CheckOpts
  | CmdBuild BuildOpts
  | CmdGraph GraphOpts
  | CmdFmt FmtOpts
  deriving (Eq, Show)

-- | Options for @skillx check@.
data CheckOpts = CheckOpts
  { coManifest :: Maybe FilePath,
    coStore :: Maybe FilePath,
    coLenient :: Bool
  }
  deriving (Eq, Show)

-- | Options for @skillx build@.
data BuildOpts = BuildOpts
  { boManifest :: Maybe FilePath,
    boStore :: Maybe FilePath,
    boHarness :: Harness,
    boOutDir :: FilePath,
    boPlace :: Maybe Text,
    boLenient :: Bool
  }
  deriving (Eq, Show)

-- | Options for @skillx graph@.
data GraphOpts = GraphOpts
  { goManifest :: Maybe FilePath,
    goStore :: Maybe FilePath,
    goPlace :: Maybe Text,
    goLenient :: Bool
  }
  deriving (Eq, Show)

-- | Options for @skillx fmt@.
data FmtOpts = FmtOpts
  { foStoreDir :: FilePath
  }
  deriving (Eq, Show)

-- | Parse CLI arguments cleanly for testing.
parseArgs :: [String] -> Maybe Command
parseArgs args =
  Opt.getParseResult (Opt.execParserPure parserPrefs parserInfo args)

parserPrefs :: ParserPrefs
parserPrefs = prefs (columns 100)

parserInfo :: ParserInfo Command
parserInfo =
  info
    (commandParser <**> helper)
    ( fullDesc
        <> header "skillx - declarative multi-harness skill graph compiler"
    )

commandParser :: Parser Command
commandParser =
  hsubparser
    ( command
        "check"
        (info (CmdCheck <$> checkOptsParser) (progDesc "Check the skill store and manifest"))
        <> command
          "build"
          (info (CmdBuild <$> buildOptsParser) (progDesc "Compile a plugin tree for a target harness"))
        <> command
          "graph"
          (info (CmdGraph <$> graphOptsParser) (progDesc "Show the resolved tier and link graph"))
        <> command
          "fmt"
          (info (CmdFmt <$> fmtOptsParser) (progDesc "Format SKILL.md files into canonical form"))
    )

manifestOptParser :: Parser (Maybe FilePath)
manifestOptParser =
  optional
    ( strOption
        ( long "manifest"
            <> short 'm'
            <> metavar "FILE"
            <> help "Path to skillx.toml (default: ./skillx.toml or $XDG_CONFIG_HOME/skillx/skillx.toml)"
        )
    )

checkOptsParser :: Parser CheckOpts
checkOptsParser =
  CheckOpts
    <$> manifestOptParser
    <*> optional
      ( strOption
          ( long "store"
              <> short 's'
              <> metavar "DIR"
              <> help "Override skill store directory"
          )
      )
    <*> switch
      ( long "lenient"
          <> help "Accept non-canonical SKILL.md header ordering"
      )

buildOptsParser :: Parser BuildOpts
buildOptsParser =
  BuildOpts
    <$> manifestOptParser
    <*> optional
      ( strOption
          ( long "store"
              <> short 's'
              <> metavar "DIR"
              <> help "Override skill store directory"
          )
      )
    <*> option
      harnessReader
      ( long "harness"
          <> short 't'
          <> metavar "HARNESS"
          <> help "Target harness: claude-code, antigravity, opencode, codex"
      )
    <*> strOption
      ( long "out"
          <> short 'o'
          <> metavar "DIR"
          <> help "Output directory for the compiled tree"
      )
    <*> optional
      ( strOption
          ( long "at"
              <> metavar "DIR"
              <> help "Include directory-scoped rules and skills for DIR"
          )
      )
    <*> switch
      ( long "lenient"
          <> help "Accept non-canonical SKILL.md header ordering"
      )

graphOptsParser :: Parser GraphOpts
graphOptsParser =
  GraphOpts
    <$> manifestOptParser
    <*> optional
      ( strOption
          ( long "store"
              <> short 's'
              <> metavar "DIR"
              <> help "Override skill store directory"
          )
      )
    <*> optional
      ( strOption
          ( long "at"
              <> metavar "DIR"
              <> help "Include directory-scoped rules and skills for DIR"
          )
      )
    <*> switch
      ( long "lenient"
          <> help "Accept non-canonical SKILL.md header ordering"
      )

fmtOptsParser :: Parser FmtOpts
fmtOptsParser =
  FmtOpts
    <$> strArgument
      ( metavar "DIR"
          <> value "skills"
          <> help "Skill store directory to format (default: skills)"
      )

harnessReader :: Opt.ReadM Harness
harnessReader =
  Opt.eitherReader $ \s ->
    case parseHarness (T.pack s) of
      Just h -> Right h
      Nothing ->
        Left "unknown harness (choose: claude-code, antigravity, opencode, codex)"

-- | Run the CLI from @main@.
runCLI :: IO ()
runCLI = do
  cmd <- customExecParser parserPrefs parserInfo
  code <- execute cmd
  exitWith code

execute :: Command -> IO ExitCode
execute = \case
  CmdCheck opts -> runCheck opts
  CmdBuild opts -> runBuild opts
  CmdGraph opts -> runGraph opts
  CmdFmt opts -> runFmt opts

resolveManifestPath :: Maybe FilePath -> IO (Either Text FilePath)
resolveManifestPath (Just p) = do
  exists <- doesFileExist p
  if exists
    then pure (Right p)
    else pure (Left ("manifest file not found: " <> T.pack p))
resolveManifestPath Nothing = do
  cwdExists <- doesFileExist "skillx.toml"
  if cwdExists
    then pure (Right "skillx.toml")
    else do
      xdgPath <- getXdgDirectory XdgConfig "skillx/skillx.toml"
      xdgExists <- doesFileExist xdgPath
      if xdgExists
        then pure (Right xdgPath)
        else pure (Left "manifest file not found: skillx.toml (or ~/.config/skillx/skillx.toml)")

loadWorkspace ::
  Maybe FilePath ->
  Maybe FilePath ->
  Bool ->
  IO (Either Text (Manifest, Map Name SkillPackage))
loadWorkspace mManifestPath mStoreOverride lenient = do
  ePath <- resolveManifestPath mManifestPath
  case ePath of
    Left err -> pure (Left err)
    Right manifestPath -> do
      bs <- BS.readFile manifestPath
      case decodeUtf8' bs of
        Left _ -> pure (Left ("manifest is not valid UTF-8: " <> T.pack manifestPath))
        Right txt -> case parseManifest txt of
          Left err -> pure (Left ("invalid manifest: " <> T.pack (show err)))
          Right manifest -> do
            let baseDir = takeDirectory manifestPath
                roots = resolveStoreRoots baseDir mStoreOverride manifest
            eStore <- loadMultiStore (not lenient) roots
            case eStore of
              Left err -> pure (Left (describeStoreError err))
              Right store -> pure (Right (manifest, store))

resolveStoreRoots :: FilePath -> Maybe FilePath -> Manifest -> [FilePath]
resolveStoreRoots _ (Just override) _ = [override]
resolveStoreRoots baseDir Nothing manifest =
  let localSources =
        [ baseDir </> p
        | LocalSource p <- Map.elems (manSources manifest)
        ]
   in if null localSources
        then [baseDir </> "skills"]
        else localSources

runCheck :: CheckOpts -> IO ExitCode
runCheck opts = do
  eWs <- loadWorkspace (coManifest opts) (coStore opts) (coLenient opts)
  case eWs of
    Left err -> dieErr err
    Right (manifest, packages) -> do
      let skills = Map.map spSkill packages
          findings = check skills manifest
      mapM_ printFinding findings
      if hasProblems findings
        then pure (ExitFailure 1)
        else do
          if null findings
            then TIO.putStrLn ("ok: " <> T.pack (show (Map.size skills)) <> " skill(s) checked")
            else pure ()
          pure ExitSuccess

runBuild :: BuildOpts -> IO ExitCode
runBuild opts = do
  eWs <- loadWorkspace (boManifest opts) (boStore opts) (boLenient opts)
  case eWs of
    Left err -> dieErr err
    Right (manifest, packages) -> do
      mPlace <- traverse makePlace (boPlace opts)
      case compile packages manifest (boHarness opts) mPlace of
        Left errs -> do
          mapM_ (\(VerificationFailed f) -> printFinding f) errs
          pure (ExitFailure 1)
        Right tree -> do
          writeFileTree (boOutDir opts) tree
          TIO.putStrLn
            ( "built "
                <> T.pack (show (Map.size (unFileTree tree)))
                <> " file(s) for "
                <> harnessText (boHarness opts)
                <> " in "
                <> T.pack (boOutDir opts)
            )
          pure ExitSuccess

runGraph :: GraphOpts -> IO ExitCode
runGraph opts = do
  eWs <- loadWorkspace (goManifest opts) (goStore opts) (goLenient opts)
  case eWs of
    Left err -> dieErr err
    Right (manifest, packages) -> do
      mPlace <- traverse makePlace (goPlace opts)
      let skills = Map.map spSkill packages
          tiers = assignTiers skills manifest mPlace
          graph = foldMap skillGraph (Map.elems skills)
      mapM_
        ( \(name, _) -> do
            let tierLabel = case Map.lookup name tiers of
                  Just TierAlways -> "always"
                  Just TierEntry -> "entry"
                  Just TierLeaf -> "leaf"
                  Nothing -> "unreachable"
                outs = Set.toAscList (Graph.successors name graph)
                arrow =
                  if null outs
                    then ""
                    else " -> " <> T.intercalate ", " (map nameText outs)
            TIO.putStrLn (nameText name <> " [" <> tierLabel <> "]" <> arrow)
        )
        (Map.toAscList skills)
      pure ExitSuccess

runFmt :: FmtOpts -> IO ExitCode
runFmt opts = do
  let root = foStoreDir opts
  entries <- sort <$> listDirectory root
  let visible = filter (not . ("." `isPrefixOf`)) entries
  results <- traverse (fmtOne root) visible
  if and results
    then pure ExitSuccess
    else pure (ExitFailure 1)
  where
    fmtOne root dir = do
      let skillFile = root </> dir </> "SKILL.md"
      exists <- doesFileExist skillFile
      if not exists
        then pure True
        else do
          bs <- BS.readFile skillFile
          case parseSkillLenient bs of
            Left err -> do
              TIO.hPutStrLn stderr ("error: " <> T.pack skillFile <> ": " <> T.pack (show err))
              pure False
            Right s -> do
              let canonical = renderSkill s
              if canonical == bs
                then pure True
                else do
                  BS.writeFile skillFile canonical
                  TIO.putStrLn ("formatted " <> T.pack skillFile)
                  pure True

makePlace :: Text -> IO Place
makePlace dir = do
  home <- T.pack <$> getHomeDirectory
  pure Place {placeHome = home, placeDir = dir}

printFinding :: Finding -> IO ()
printFinding f =
  TIO.hPutStrLn
    stderr
    ( severityText (findingSeverity f)
        <> " ["
        <> kindText (findingKind f)
        <> "] "
        <> findingDetail f
    )

dieErr :: Text -> IO ExitCode
dieErr msg = do
  TIO.hPutStrLn stderr ("error: " <> msg)
  pure (ExitFailure 1)
