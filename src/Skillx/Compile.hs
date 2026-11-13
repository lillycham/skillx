{-# LANGUAGE LambdaCase #-}

-- | Pure ahead-of-time compilation of a skill store and manifest into a
-- harness-specific plugin or directory tree.
--
-- Disclosure tiers:
--
-- * Tier 0 (@always@): emitted as always-on rules (@rules/<name>.md@ for
--   Claude Code and Antigravity, or combined into @AGENTS.md@ for OpenCode and
--   Codex).
-- * Tier 1 (@entry@): emitted as top-level skills (@skills/<name>/SKILL.md@),
--   exposing @name@ and @description@ in the harness system prompt.
-- * Tier 2 (reachable via @links@ from Tier 0 or Tier 1, and not itself a
--   root): emitted under @_nodes/<name>/SKILL.md@, omitted from the top-level
--   system prompt and reached via the compiled @## Linked skills@ routing
--   footer of the skills that link to it.
module Skillx.Compile
  ( -- * Types
    Harness (..),
    parseHarness,
    harnessText,
    allHarnesses,
    Tier (..),
    SkillPackage (..),
    FileTree (..),
    CompileError (..),

    -- * Compilation
    compile,
    assignTiers,
    renderHarnessSkill,
  )
where

import Data.Aeson (object, (.=))
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BL
import Data.List (nub)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Skillx.Check (Finding (findingSeverity), Severity (Problem), check)
import Skillx.Graph qualified as Graph
import Skillx.Manifest
  ( Manifest (manAlways, manEntry, manName, manVersion),
    Place,
    PlaceRules (PlaceRules, prAlways, prEntry),
    matchingPlaceRules,
  )
import Skillx.Name (Name, nameText)
import Skillx.Skill (Skill (skillBody, skillDescription, skillLinks, skillName), skillGraph)

-- | Supported target agent harnesses.
data Harness
  = ClaudeCode
  | Antigravity
  | OpenCode
  | Codex
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | All supported harnesses.
allHarnesses :: [Harness]
allHarnesses = [minBound .. maxBound]

-- | Parse a harness identifier.
parseHarness :: Text -> Maybe Harness
parseHarness = \case
  "claude" -> Just ClaudeCode
  "claude-code" -> Just ClaudeCode
  "antigravity" -> Just Antigravity
  "agy" -> Just Antigravity
  "opencode" -> Just OpenCode
  "codex" -> Just Codex
  _ -> Nothing

-- | Canonical kebab-case name of a harness.
harnessText :: Harness -> Text
harnessText = \case
  ClaudeCode -> "claude-code"
  Antigravity -> "antigravity"
  OpenCode -> "opencode"
  Codex -> "codex"

-- | The compiled disclosure tier of a skill.
data Tier
  = -- | Tier 0: always-on rule.
    TierAlways
  | -- | Tier 1: top-level entry skill in the system prompt.
    TierEntry
  | -- | Tier 2: reachable leaf node unfolded via parent routing tables.
    TierLeaf
  deriving (Eq, Ord, Show)

-- | A skill together with any auxiliary files in its directory (for example
-- @scripts/...@ or @references/...@), keyed by relative path inside the skill
-- directory.
data SkillPackage = SkillPackage
  { spSkill :: Skill,
    spFiles :: Map FilePath ByteString
  }
  deriving (Eq, Show)

-- | An in-memory file tree produced by 'compile', keyed by relative path from
-- the output root.
newtype FileTree = FileTree
  { unFileTree :: Map FilePath ByteString
  }
  deriving (Eq, Show)

-- | Why 'compile' refuses to emit a file tree.
data CompileError
  = -- | Static verification found a 'Problem' (missing root, dangling link, or
    -- missing MCP/binary requirement).
    VerificationFailed Finding
  deriving (Eq, Show)

-- | Compute the active disclosure tier for every reachable skill in the store.
--
-- When @Maybe Place@ is @Just p@, any matching @[places.<dir>]@ entries in the
-- manifest are merged with the global @always@ and @entry@ roots. If a skill
-- is named in both @always@ and @entry@, @TierAlways@ wins.
assignTiers ::
  Map Name Skill ->
  Manifest ->
  Maybe Place ->
  Map Name Tier
assignTiers store manifest mPlace =
  Map.fromList
    ( [(n, TierLeaf) | n <- Set.toAscList leaves]
        ++ [(n, TierEntry) | n <- Set.toAscList entrySet]
        ++ [(n, TierAlways) | n <- Set.toAscList alwaysSet]
    )
  where
    placeRules = maybe (PlaceRules [] []) (`matchingPlaceRules` manifest) mPlace
    alwaysList = nub (manAlways manifest ++ prAlways placeRules)
    entryList = nub (manEntry manifest ++ prEntry placeRules)
    alwaysSet = Set.fromList alwaysList `Set.intersection` Map.keysSet store
    entrySet =
      (Set.fromList entryList `Set.intersection` Map.keysSet store)
        `Set.difference` alwaysSet
    roots = alwaysSet <> entrySet
    graph = foldMap skillGraph (Map.elems store)
    reached = Graph.reachable roots graph `Set.intersection` Map.keysSet store
    leaves = reached `Set.difference` roots

-- | Compile a skill store and manifest for a target 'Harness'.
--
-- Returns 'Left' if static verification reports any 'Problem' findings.
compile ::
  Map Name SkillPackage ->
  Manifest ->
  Harness ->
  Maybe Place ->
  Either [CompileError] FileTree
compile packages manifest harness mPlace =
  case filter ((== Problem) . findingSeverity) (check skills manifest) of
    problems@(_ : _) -> Left (map VerificationFailed problems)
    [] -> Right (FileTree (manifestFiles <> alwaysFiles <> skillNodeFiles))
  where
    skills :: Map Name Skill
    skills = Map.map spSkill packages

    tiers :: Map Name Tier
    tiers = assignTiers skills manifest mPlace

    placeRules :: PlaceRules
    placeRules = maybe (PlaceRules [] []) (`matchingPlaceRules` manifest) mPlace

    orderedAlways :: [Name]
    orderedAlways =
      filter (`Map.member` skills) $
        nub (manAlways manifest ++ prAlways placeRules)

    -- Path where a skill's primary markdown file lives in the output tree.
    primaryPath :: Name -> Tier -> FilePath
    primaryPath name = \case
      TierAlways -> case harness of
        ClaudeCode -> "rules/" <> T.unpack (nameText name) <> ".md"
        Antigravity -> "rules/" <> T.unpack (nameText name) <> ".md"
        OpenCode -> "AGENTS.md"
        Codex -> "AGENTS.md"
      TierEntry -> "skills/" <> T.unpack (nameText name) <> "/SKILL.md"
      TierLeaf -> "_nodes/" <> T.unpack (nameText name) <> "/SKILL.md"

    -- Relative path from a skill at a given tier to a linked target skill.
    relLinkPath :: Tier -> Name -> Tier -> Text
    relLinkPath fromTier targetName toTier =
      let prefix = case fromTier of
            TierAlways -> case harness of
              ClaudeCode -> "../"
              Antigravity -> "../"
              OpenCode -> ""
              Codex -> ""
            TierEntry -> "../../"
            TierLeaf -> "../../"
       in prefix <> T.pack (primaryPath targetName toTier)

    resolvedLinksFor :: Tier -> Skill -> [(Name, Text, Text)]
    resolvedLinksFor fromTier s =
      [ (targetName, skillDescription targetSkill, relLinkPath fromTier targetName targetTier)
      | targetName <- Set.toAscList (skillLinks s),
        Just targetSkill <- [Map.lookup targetName skills],
        Just targetTier <- [Map.lookup targetName tiers]
      ]

    manifestFiles :: Map FilePath ByteString
    manifestFiles = case harness of
      ClaudeCode ->
        Map.singleton ".claude-plugin/plugin.json" (pluginJsonBytes manifest)
      Antigravity ->
        Map.singleton "plugin.json" (pluginJsonBytes manifest)
      OpenCode -> Map.empty
      Codex -> Map.empty

    alwaysFiles :: Map FilePath ByteString
    alwaysFiles = case harness of
      ClaudeCode ->
        Map.fromList
          [ ( primaryPath name TierAlways,
              renderAlwaysRule s (resolvedLinksFor TierAlways s)
            )
          | name <- orderedAlways,
            Just s <- [Map.lookup name skills]
          ]
      Antigravity ->
        Map.fromList
          [ ( primaryPath name TierAlways,
              renderAlwaysRule s (resolvedLinksFor TierAlways s)
            )
          | name <- orderedAlways,
            Just s <- [Map.lookup name skills]
          ]
      OpenCode -> combinedAgentsMd orderedAlways
      Codex -> combinedAgentsMd orderedAlways

    combinedAgentsMd :: [Name] -> Map FilePath ByteString
    combinedAgentsMd names =
      let blocks =
            [ renderAlwaysRuleText s (resolvedLinksFor TierAlways s)
            | name <- names,
              Just s <- [Map.lookup name skills]
            ]
       in if null blocks
            then Map.empty
            else Map.singleton "AGENTS.md" (encodeUtf8 (T.intercalate "\n" blocks))

    skillNodeFiles :: Map FilePath ByteString
    skillNodeFiles =
      Map.unions
        [ emitNode name pkg tier
        | (name, tier) <- Map.toAscList tiers,
          tier /= TierAlways,
          Just pkg <- [Map.lookup name packages]
        ]

    emitNode :: Name -> SkillPackage -> Tier -> Map FilePath ByteString
    emitNode name pkg tier =
      let s = spSkill pkg
          dirPrefix = case tier of
            TierEntry -> "skills/" <> T.unpack (nameText name)
            TierLeaf -> "_nodes/" <> T.unpack (nameText name)
            TierAlways -> "rules"
          skillPath = dirPrefix <> "/SKILL.md"
          skillBytes = renderHarnessSkill s (resolvedLinksFor tier s)
          auxFiles =
            Map.mapKeys
              (\rel -> dirPrefix <> "/" <> rel)
              (spFiles pkg)
       in Map.insert skillPath skillBytes auxFiles

-- | Render a compiled @SKILL.md@ for a target harness: standard @name@ and
-- @description@ frontmatter, the skill body, and an optional generated
-- @## Linked skills@ routing footer.
renderHarnessSkill :: Skill -> [(Name, Text, Text)] -> ByteString
renderHarnessSkill s links =
  encodeUtf8 (frontmatter <> "\n" <> bodyWithFooter (skillBody s) links)
  where
    frontmatter =
      T.unlines
        [ "---",
          "name: " <> nameText (skillName s),
          "description: " <> skillDescription s,
          "---"
        ]

renderAlwaysRule :: Skill -> [(Name, Text, Text)] -> ByteString
renderAlwaysRule s links = encodeUtf8 (renderAlwaysRuleText s links)

renderAlwaysRuleText :: Skill -> [(Name, Text, Text)] -> Text
renderAlwaysRuleText s links = bodyWithFooter (skillBody s) links

bodyWithFooter :: Text -> [(Name, Text, Text)] -> Text
bodyWithFooter body [] = ensureTrailingNewline body
bodyWithFooter body links =
  ensureTrailingNewline body
    <> "\n## Linked skills\n\n"
    <> T.unlines
      [ "- [" <> nameText target <> "](" <> path <> "): " <> desc
      | (target, desc, path) <- links
      ]

ensureTrailingNewline :: Text -> Text
ensureTrailingNewline t
  | T.null t = "\n"
  | "\n" `T.isSuffixOf` t = t
  | otherwise = t <> "\n"

pluginJsonBytes :: Manifest -> ByteString
pluginJsonBytes m =
  BL.toStrict
    ( Aeson.encode
        ( object
            [ "name" .= manName m,
              "version" .= manVersion m,
              "description" .= ("Compiled by skillx (" <> manName m <> ")")
            ]
        )
    )
    <> "\n"
