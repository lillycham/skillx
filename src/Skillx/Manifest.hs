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
module Skillx.Manifest
  ( -- * Types
    Manifest (..),
    PlaceRules (..),
    SourceSpec (..),
    Place (..),
    ManifestError (..),

    -- * Parsing
    parseManifest,

    -- * Queries
    allRoots,
    globalRoots,
    matchingPlaceRules,
  )
where

import Data.Bifunctor (first)
import Data.List (nub)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
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
    -- | MCP servers available in the target environment (if @Just@, checked by
    -- @skillx check@).
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
  | -- | A skill, MCP or binary name in the manifest is not a valid 'Name'.
    BadManifestName Text Text NameError
  | -- | A @[sources.<name>]@ entry specifies neither @path@ nor @git@, or both.
    BadSourceSpec Text
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
