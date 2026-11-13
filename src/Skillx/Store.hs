{-# LANGUAGE LambdaCase #-}

-- | Loading skill directories from disk and writing compiled 'FileTree's.
module Skillx.Store
  ( StoreError (..),
    describeStoreError,
    loadSkillDir,
    loadStore,
    loadMultiStore,
    writeFileTree,
  )
where

import Control.Exception (IOException, try)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.List (isPrefixOf, sort)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Skillx.Compile (FileTree (FileTree), SkillPackage (SkillPackage, spSkill))
import Skillx.Name (Name, nameText)
import Skillx.Skill
  ( Skill (skillName),
    SkillParseError,
    parseSkill,
    parseSkillLenient,
  )
import System.Directory
  ( createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    listDirectory,
  )
import System.FilePath
  ( dropTrailingPathSeparator,
    makeRelative,
    takeDirectory,
    takeFileName,
    (</>),
  )

-- | Errors that can occur when loading skills from the filesystem.
data StoreError
  = -- | A filesystem read failed.
    StoreIoError FilePath Text
  | -- | A @SKILL.md@ file failed to parse.
    StoreParseError FilePath SkillParseError
  | -- | The directory name does not match the skill's @name@ header.
    DirectoryNameMismatch FilePath Text Text
  | -- | Two sources define a skill with the same 'Name'.
    DuplicateSkillName Name FilePath FilePath
  deriving (Eq, Show)

-- | Render a 'StoreError' as a human-readable message.
describeStoreError :: StoreError -> Text
describeStoreError = \case
  StoreIoError p msg ->
    "cannot read " <> T.pack p <> ": " <> msg
  StoreParseError p err ->
    "invalid skill file " <> T.pack p <> ": " <> T.pack (show err)
  DirectoryNameMismatch p dirName hdrName ->
    "directory name "
      <> dirName
      <> " at "
      <> T.pack p
      <> " does not match skill name header "
      <> hdrName
  DuplicateSkillName n p1 p2 ->
    "duplicate skill "
      <> nameText n
      <> " in "
      <> T.pack p1
      <> " and "
      <> T.pack p2

-- | Load one skill directory containing @SKILL.md@ and any auxiliary files.
--
-- When the boolean flag is 'True', 'parseSkill' (strict canonical form) is
-- used; when 'False', 'parseSkillLenient' is used.
loadSkillDir :: Bool -> FilePath -> IO (Either StoreError SkillPackage)
loadSkillDir strict dir = do
  let skillFile = dir </> "SKILL.md"
  eBytes <- tryIo skillFile (BS.readFile skillFile)
  case eBytes of
    Left err -> pure (Left err)
    Right bs -> do
      let parser = if strict then parseSkill else parseSkillLenient
      case first (StoreParseError skillFile) (parser bs) of
        Left err -> pure (Left err)
        Right skill -> do
          let dirBase = T.pack (takeFileName (dropTrailingPathSeparator dir))
              hdrName = nameText (skillName skill)
          if dirBase /= hdrName
            then pure (Left (DirectoryNameMismatch dir dirBase hdrName))
            else do
              eAux <- collectAuxFiles dir dir
              pure (SkillPackage skill <$> eAux)

collectAuxFiles ::
  FilePath ->
  FilePath ->
  IO (Either StoreError (Map FilePath ByteString))
collectAuxFiles root current = do
  eEntries <- tryIo current (sort <$> listDirectory current)
  case eEntries of
    Left err -> pure (Left err)
    Right entries -> do
      let visible = filter (not . ("." `isPrefixOf`)) entries
      go visible Map.empty
  where
    go [] acc = pure (Right acc)
    go (e : es) acc = do
      let full = current </> e
          rel = makeRelative root full
      if rel == "SKILL.md"
        then go es acc
        else do
          isDir <- doesDirectoryExist full
          if isDir
            then do
              eSub <- collectAuxFiles root full
              case eSub of
                Left err -> pure (Left err)
                Right subMap -> go es (Map.union acc subMap)
            else do
              isFile <- doesFileExist full
              if not isFile
                then go es acc
                else do
                  eBs <- tryIo full (BS.readFile full)
                  case eBs of
                    Left err -> pure (Left err)
                    Right bs -> go es (Map.insert rel bs acc)

-- | Load all skill directories under a single store root directory.
loadStore :: Bool -> FilePath -> IO (Either StoreError (Map Name SkillPackage))
loadStore strict root = do
  eEntries <- tryIo root (sort <$> listDirectory root)
  case eEntries of
    Left err -> pure (Left err)
    Right entries -> do
      let visible = filter (not . ("." `isPrefixOf`)) entries
      go visible Map.empty
  where
    go [] acc = pure (Right acc)
    go (e : es) acc = do
      let subdir = root </> e
          skillFile = subdir </> "SKILL.md"
      hasSkill <- doesFileExist skillFile
      if not hasSkill
        then go es acc
        else do
          ePkg <- loadSkillDir strict subdir
          case ePkg of
            Left err -> pure (Left err)
            Right pkg ->
              go es (Map.insert (skillName (spSkill pkg)) pkg acc)

-- | Load and merge skill directories from multiple store roots, failing on
-- duplicate skill names.
loadMultiStore ::
  Bool ->
  [FilePath] ->
  IO (Either StoreError (Map Name SkillPackage))
loadMultiStore strict roots = go roots Map.empty
  where
    go [] acc = pure (Right (Map.map fst acc))
    go (r : rs) acc = do
      eStore <- loadStore strict r
      case eStore of
        Left err -> pure (Left err)
        Right m -> case merge r acc (Map.toAscList m) of
          Left err -> pure (Left err)
          Right acc' -> go rs acc'
    merge _ acc [] = Right acc
    merge r acc ((n, pkg) : rest) =
      case Map.lookup n acc of
        Just (_, prevRoot) -> Left (DuplicateSkillName n prevRoot r)
        Nothing -> merge r (Map.insert n (pkg, r) acc) rest

-- | Write every file in a 'FileTree' under the given output directory.
writeFileTree :: FilePath -> FileTree -> IO ()
writeFileTree outRoot (FileTree files) =
  mapM_ writeOne (Map.toAscList files)
  where
    writeOne (relPath, bs) = do
      let fullPath = outRoot </> relPath
      createDirectoryIfMissing True (takeDirectory fullPath)
      BS.writeFile fullPath bs

tryIo :: FilePath -> IO a -> IO (Either StoreError a)
tryIo p act =
  first (StoreIoError p . T.pack . show) <$> try @IOException act
