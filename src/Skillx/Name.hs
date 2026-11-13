-- | Validated names for skills, MCP servers and required binaries.
--
-- A 'Name' becomes a directory or file name on disk and a skill identifier in
-- agent harnesses, so the constructor is not exported. Make a 'Name' with
-- 'mkName'.
module Skillx.Name
  ( Name,
    NameError (..),
    mkName,
    nameText,
  )
where

import Data.Char (isAsciiLower, isDigit)
import Data.Text (Text)
import Data.Text qualified as T

-- | A validated identifier, for example @rust-dev@ or @asd-ste100@.
newtype Name = Name Text
  deriving (Eq, Ord, Show)

-- | Why a text is not a valid 'Name'.
data NameError
  = -- | The text is empty.
    EmptyName
  | -- | The text is longer than 64 characters.
    NameTooLong Text
  | -- | The text starts with a character other than @a@ to @z@ or @0@ to @9@,
    -- or contains a character outside @a@ to @z@, @0@ to @9@, @-@, @_@ and @.@.
    BadName Text
  deriving (Eq, Show)

-- | Validate a text and construct a 'Name'.
--
-- Rules:
--
-- * 1 to 64 characters;
-- * starts with @a@ to @z@ or @0@ to @9@ (never @.@ or @-@);
-- * contains only @a@ to @z@, @0@ to @9@, @-@, @_@ and @.@.
mkName :: Text -> Either NameError Name
mkName t
  | T.null t = Left EmptyName
  | T.length t > 64 = Left (NameTooLong t)
  | not (okStart (T.take 1 t)) || not (T.all okChar t) = Left (BadName t)
  | otherwise = Right (Name t)
  where
    okStart s = T.all (\c -> isAsciiLower c || isDigit c) s
    okChar c = isAsciiLower c || isDigit c || c `elem` ("-_." :: String)

-- | The underlying text of a 'Name'. @mkName (nameText n) == Right n@.
nameText :: Name -> Text
nameText (Name t) = t
