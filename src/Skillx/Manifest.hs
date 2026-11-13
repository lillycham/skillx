{-# LANGUAGE LambdaCase #-}

-- | The declarative manifest of a skillx workspace (@skillx.toml@).
--
-- This module is pure: it parses the TOML manifest that assigns root disclosure
-- tiers (@always@, @entry@, and directory-scoped @places@), external or local
-- skill sources, and available environment capabilities (@mcp@, @bins@).
--
-- Example @skillx.toml@:
--
-- > name = "my-skills"
-- > version = "0.1.0"
-- > always = ["core-rules"]
-- > entry = ["rust-dev", "git-finish"]
-- > available-mcp = ["akyuu"]
-- > available-bins = ["cargo", "rustc", "git"]
-- >
-- > [places."~/devel/garden-app"]
-- > always = ["garden-rules"]
-- > entry = ["garden-deploy"]
-- >
-- > [sources.local]
-- > path = "./skills"
-- >
-- > [sources.upstream]
-- > git = "https://example.com/skills.git"
-- > rev = "a1b2c3d"
-- > subdir = "skills"
-- > [mcp.akyuu]
-- > command = "akyuu"
-- > args = ["--root", "/path/to/store", "mcp"]
-- >
-- > [hooks.akyuu-context]
-- > event = "SessionStart"
-- > command = "akyuu context --page 1"
-- > inject = true
module Skillx.Manifest
  ( -- * Types
    Manifest (..),
    PlaceRules (..),
    SourceSpec (..),
    McpServer (..),
    HookEvent (..),
    HookSpec (..),
    Place (..),
    ManifestError (..),

    -- * Parsing
    parseManifest,
    parseHookEvent,
    hookEventText,

    -- * Queries
    allRoots,
    globalRoots,
    matchingPlaceRules,
    knownMcp,
    knownHooks,
  )
where

import Data.Bifunctor (first)
import Data.List (nub)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isNothing)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Skillx.Name (Name, NameError, mkName)
import Toml (Result (Failure, Success), decode)
import Toml.Schema (FromValue (fromValue), optKey, parseTableFromValue)

-- | A parsed @skillx.toml@ manifest.
data Manifest = Manifest
  { -- | Plugin or bundle name (defaults to @"skillx-bundle"@).
    manName :: Text,
    -- | Plugin version string (defaults to @"0.1.0"@).
    manVersion :: Text,
    -- | Tier 0 skills: full body is emitted as always-on rules.
    manAlways :: [Name],
    -- | Tier 1 skills: emitted as top-level entry skills in the harness.
    manEntry :: [Name],
    -- | Directory-scoped Tier 0 and Tier 1 additions, in manifest key order.
    manPlaces :: Map Text PlaceRules,
    -- | Declared skill sources, keyed by source name.
    manSources :: Map Text SourceSpec,
    -- | MCP server configurations, keyed by server name.
    manMcp :: Map Name McpServer,
    -- | Lifecycle hook configurations, keyed by hook name.
    manHooks :: Map Name HookSpec,
    -- | Extra MCP servers available in the target environment (merged with
    -- @manMcp@ keys by 'knownMcp').
    manAvailableMcp :: Maybe (Set Name),
    -- | Binaries available on @PATH@ in the target environment (if @Just@,
    -- checked by @skillx check@).
    manAvailableBins :: Maybe (Set Name)
  }
  deriving (Eq, Show)

-- | Tier 0 (@always@) and Tier 1 (@entry@) roots active in a directory.
data PlaceRules = PlaceRules
  { prAlways :: [Name],
    prEntry :: [Name]
  }
  deriving (Eq, Show)

-- | Where a source of skills lives.
data SourceSpec
  = -- | A local filesystem directory containing skill folders.
    LocalSource FilePath
  | -- | A Git repository, pinned revision, and optional subdirectory.
    GitSource
      { gsUrl :: Text,
        gsRev :: Maybe Text,
        gsSubdir :: Maybe FilePath
      }
  deriving (Eq, Show)

-- | An MCP server definition in @[mcp.<name>]@.
data McpServer
  = -- | A local stdio MCP server process.
    McpStdio
      { mcpCommand :: Text,
        mcpArgs :: [Text],
        mcpEnv :: Map Text Text
      }
  | -- | A remote Streamable HTTP MCP server endpoint.
    McpHttp
      { mcpUrl :: Text
      }
  deriving (Eq, Show)

-- | Lifecycle events at which a hook can run.
data HookEvent
  = SessionStart
  | PreToolUse
  | PostToolUse
  | PreInvocation
  | PostInvocation
  | Stop
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Parse a 'HookEvent' name.
parseHookEvent :: Text -> Maybe HookEvent
parseHookEvent = \case
  "SessionStart" -> Just SessionStart
  "PreToolUse" -> Just PreToolUse
  "PostToolUse" -> Just PostToolUse
  "PreInvocation" -> Just PreInvocation
  "UserPromptSubmit" -> Just PreInvocation
  "PostInvocation" -> Just PostInvocation
  "Stop" -> Just Stop
  _ -> Nothing

-- | Canonical event name for 'HookEvent'.
hookEventText :: HookEvent -> Text
hookEventText = \case
  SessionStart -> "SessionStart"
  PreToolUse -> "PreToolUse"
  PostToolUse -> "PostToolUse"
  PreInvocation -> "PreInvocation"
  PostInvocation -> "PostInvocation"
  Stop -> "Stop"

-- | A lifecycle hook definition in @[hooks.<name>]@.
data HookSpec = HookSpec
  { -- | Lifecycle event when the hook fires.
    hookEvent :: HookEvent,
    -- | Shell command to execute.
    hookCommand :: Text,
    -- | Optional tool matcher regex (used by @PreToolUse@ and @PostToolUse@).
    hookMatcher :: Maybe Text,
    -- | When @True@, the command's plain-text @stdout@ is injected as a
    -- system message (wrapped automatically for harnesses that expect JSON).
    hookInject :: Bool,
    -- | Optional timeout in seconds.
    hookTimeout :: Maybe Int
  }
  deriving (Eq, Show)

-- | A working directory context for evaluating directory-scoped @places@.
data Place = Place
  { placeHome :: Text,
    placeDir :: Text
  }
  deriving (Eq, Show)

-- | Why 'parseManifest' rejects a @skillx.toml@ document.
data ManifestError
  = -- | The TOML syntax or schema is invalid.
    TomlError [Text]
  | -- | A skill, MCP, hook or binary name in the manifest is not a valid 'Name'.
    BadManifestName Text Text NameError
  | -- | A @[sources.<name>]@ entry specifies neither @path@ nor @git@, or both.
    BadSourceSpec Text
  | -- | A @[mcp.<name>]@ entry specifies neither @command@ nor @url@, or both.
    BadMcpSpec Text
  | -- | A @[hooks.<name>]@ entry has an unknown @event@, empty @command@, or
    -- non-positive @timeout@.
    BadHookSpec Text
  | -- | The plugin @name@ or @version@ is empty.
    EmptyMetadata Text
  deriving (Eq, Show)

-- Raw TOML schema types --------------------------------------------------

data RawManifest = RawManifest
  { rmName :: Maybe Text,
    rmVersion :: Maybe Text,
    rmAlways :: Maybe [Text],
    rmEntry :: Maybe [Text],
    rmPlaces :: Maybe (Map Text RawPlace),
    rmSources :: Maybe (Map Text RawSource),
    rmMcp :: Maybe (Map Text RawMcp),
    rmHooks :: Maybe (Map Text RawHook),
    rmAvailableMcp :: Maybe [Text],
    rmAvailableBins :: Maybe [Text]
  }

data RawPlace = RawPlace
  { rpAlways :: Maybe [Text],
    rpEntry :: Maybe [Text]
  }

data RawSource = RawSource
  { rsPath :: Maybe Text,
    rsGit :: Maybe Text,
    rsRev :: Maybe Text,
    rsSubdir :: Maybe Text
  }

data RawMcp = RawMcp
  { rmcpCommand :: Maybe Text,
    rmcpArgs :: Maybe [Text],
    rmcpEnv :: Maybe (Map Text Text),
    rmcpUrl :: Maybe Text
  }

data RawHook = RawHook
  { rhEvent :: Maybe Text,
    rhCommand :: Maybe Text,
    rhMatcher :: Maybe Text,
    rhInject :: Maybe Bool,
    rhTimeout :: Maybe Int
  }

instance FromValue RawManifest where
  fromValue =
    parseTableFromValue $
      RawManifest
        <$> optKey "name"
        <*> optKey "version"
        <*> optKey "always"
        <*> optKey "entry"
        <*> optKey "places"
        <*> optKey "sources"
        <*> optKey "mcp"
        <*> optKey "hooks"
        <*> optKey "available-mcp"
        <*> optKey "available-bins"

instance FromValue RawPlace where
  fromValue =
    parseTableFromValue $
      RawPlace
        <$> optKey "always"
        <*> optKey "entry"

instance FromValue RawSource where
  fromValue =
    parseTableFromValue $
      RawSource
        <$> optKey "path"
        <*> optKey "git"
        <*> optKey "rev"
        <*> optKey "subdir"

instance FromValue RawMcp where
  fromValue =
    parseTableFromValue $
      RawMcp
        <$> optKey "command"
        <*> optKey "args"
        <*> optKey "env"
        <*> optKey "url"

instance FromValue RawHook where
  fromValue =
    parseTableFromValue $
      RawHook
        <$> optKey "event"
        <*> optKey "command"
        <*> optKey "matcher"
        <*> optKey "inject"
        <*> optKey "timeout"

-- | Parse a @skillx.toml@ document.
parseManifest :: Text -> Either ManifestError Manifest
parseManifest input =
  case decode input of
    Failure errs -> Left (TomlError (map T.pack errs))
    Success _ raw -> validateRaw raw

validateRaw :: RawManifest -> Either ManifestError Manifest
validateRaw rm = do
  let name = fromMaybe "skillx-bundle" (rmName rm)
      version = fromMaybe "0.1.0" (rmVersion rm)
  if T.null (T.strip name)
    then Left (EmptyMetadata "name")
    else pure ()
  if T.null (T.strip version)
    then Left (EmptyMetadata "version")
    else pure ()
  always <- parseNameList "always" (fromMaybe [] (rmAlways rm))
  entry <- parseNameList "entry" (fromMaybe [] (rmEntry rm))
  places <-
    Map.fromAscList
      <$> traverse
        (\(k, rp) -> (k,) <$> validatePlace k rp)
        (Map.toAscList (fromMaybe Map.empty (rmPlaces rm)))
  sources <-
    Map.fromAscList
      <$> traverse
        (\(k, rs) -> (k,) <$> validateSource k rs)
        (Map.toAscList (fromMaybe Map.empty (rmSources rm)))
  mcp <-
    Map.fromList
      <$> traverse
        (\(k, r) -> validateMcp k r)
        (Map.toAscList (fromMaybe Map.empty (rmMcp rm)))
  hooks <-
    Map.fromList
      <$> traverse
        (\(k, r) -> validateHook k r)
        (Map.toAscList (fromMaybe Map.empty (rmHooks rm)))
  availMcp <-
    traverse
      (fmap Set.fromList . parseNameList "available-mcp")
      (rmAvailableMcp rm)
  availBins <-
    traverse
      (fmap Set.fromList . parseNameList "available-bins")
      (rmAvailableBins rm)
  Right
    Manifest
      { manName = name,
        manVersion = version,
        manAlways = nub always,
        manEntry = nub entry,
        manPlaces = places,
        manSources = sources,
        manMcp = mcp,
        manHooks = hooks,
        manAvailableMcp = availMcp,
        manAvailableBins = availBins
      }
  where
    parseNameList field rawNames =
      traverse (\r -> first (BadManifestName field r) (mkName r)) rawNames
    validatePlace k rp = do
      a <- parseNameList ("places." <> k <> ".always") (fromMaybe [] (rpAlways rp))
      e <- parseNameList ("places." <> k <> ".entry") (fromMaybe [] (rpEntry rp))
      Right PlaceRules {prAlways = nub a, prEntry = nub e}
    validateSource k rs =
      case (rsPath rs, rsGit rs) of
        (Just p, Nothing)
          | not (T.null (T.strip p)) ->
              Right (LocalSource (T.unpack p))
        (Nothing, Just g)
          | not (T.null (T.strip g)) ->
              Right
                GitSource
                  { gsUrl = g,
                    gsRev = rsRev rs,
                    gsSubdir = T.unpack <$> rsSubdir rs
                  }
        _ -> Left (BadSourceSpec k)
    validateMcp k r = do
      n <- first (BadManifestName "mcp" k) (mkName k)
      case (rmcpCommand r, rmcpUrl r) of
        (Just cmd, Nothing)
          | not (T.null (T.strip cmd)) ->
              Right
                ( n,
                  McpStdio
                    { mcpCommand = cmd,
                      mcpArgs = fromMaybe [] (rmcpArgs r),
                      mcpEnv = fromMaybe Map.empty (rmcpEnv r)
                    }
                )
        (Nothing, Just u)
          | not (T.null (T.strip u))
              && isNothing (rmcpArgs r)
              && isNothing (rmcpEnv r) ->
              Right (n, McpHttp {mcpUrl = u})
        _ -> Left (BadMcpSpec k)
    validateHook k r = do
      n <- first (BadManifestName "hooks" k) (mkName k)
      evRaw <- maybe (Left (BadHookSpec k)) Right (rhEvent r)
      ev <- maybe (Left (BadHookSpec k)) Right (parseHookEvent evRaw)
      cmd <- maybe (Left (BadHookSpec k)) Right (rhCommand r)
      if T.null (T.strip cmd)
        then Left (BadHookSpec k)
        else pure ()
      case rhTimeout r of
        Just t | t <= 0 -> Left (BadHookSpec k)
        _ -> pure ()
      Right
        ( n,
          HookSpec
            { hookEvent = ev,
              hookCommand = cmd,
              hookMatcher = rhMatcher r,
              hookInject = fromMaybe False (rhInject r),
              hookTimeout = rhTimeout r
            }
        )

-- | Known MCP servers from @[mcp.<name>]@ and @available-mcp@, or @Nothing@
-- when neither is configured.
knownMcp :: Manifest -> Maybe (Set Name)
knownMcp m
  | Map.null (manMcp m) && isNothing (manAvailableMcp m) = Nothing
  | otherwise =
      Just (Map.keysSet (manMcp m) <> fromMaybe Set.empty (manAvailableMcp m))

-- | Known hooks from @[hooks.<name>]@.
knownHooks :: Manifest -> Set Name
knownHooks = Map.keysSet . manHooks

-- | The global root skills (@always@ and @entry@), excluding @places@.
globalRoots :: Manifest -> Set Name
globalRoots m = Set.fromList (manAlways m ++ manEntry m)

-- | Every root skill named anywhere in the manifest (@always@, @entry@, and all
-- @places@).
allRoots :: Manifest -> Set Name
allRoots m =
  globalRoots m
    <> foldMap
      (\pr -> Set.fromList (prAlways pr ++ prEntry pr))
      (Map.elems (manPlaces m))

-- | Combined 'PlaceRules' for all @[places.<dir>]@ entries that contain the
-- given 'Place'.
matchingPlaceRules :: Place -> Manifest -> PlaceRules
matchingPlaceRules p m =
  PlaceRules
    { prAlways = nub (concat [prAlways pr | (d, pr) <- Map.toAscList (manPlaces m), contains p d]),
      prEntry = nub (concat [prEntry pr | (d, pr) <- Map.toAscList (manPlaces m), contains p d])
    }

contains :: Place -> Text -> Bool
contains p d
  | d == "*" = True
  | otherwise = here == dir || (dir <> "/") `T.isPrefixOf` here
  where
    dir = T.dropWhileEnd (== '/') (expand d)
    here = T.dropWhileEnd (== '/') (expand (placeDir p))
    expand x = case T.stripPrefix "~" x of
      Just rest | T.null rest || "/" `T.isPrefixOf` rest -> placeHome p <> rest
      _ -> x
