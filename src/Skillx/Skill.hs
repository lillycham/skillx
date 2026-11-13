-- | A skill: one procedural node of the store.
--
-- This module is pure. It turns a 'Skill' into bytes and bytes into a 'Skill'.
--
-- The canonical source format is a header block fenced by @---@ lines, one
-- blank line, and the Markdown body:
--
-- > ---
-- > name: rust-dev
-- > description: Rust conventions and entry point to the other Rust skills.
-- > links: rust-cargo, rust-debug
-- > requires-mcp: akyuu
-- > requires-hook: akyuu-context
-- > requires-bin: cargo, rustc
-- > ---
-- >
-- > The body is free Markdown text.
--
-- Rules of the format:
--
-- * @name@ and @description@ are required and appear once, in that order.
-- * @links@, @requires-mcp@, @requires-hook@ and @requires-bin@ are optional.
--   Each appears at most once, in that order, only when its set is non-empty,
--   with values in ascending order separated by @", "@.
-- * For every skill @s@ constructed with 'mkSkill',
--   @parseSkill (renderSkill s) == Right s@, and for every @bs@ accepted by
--   'parseSkill', @renderSkill s == bs@.
module Skillx.Skill
  ( -- * Skill
    Skill
      ( skillName,
        skillDescription,
        skillLinks,
        skillRequiresMcp,
        skillRequiresHook,
        skillRequiresBin,
        skillBody
      ),
    SkillError (..),
    mkSkill,
    validDescription,

    -- * Parsing and rendering
    SkillParseError (..),
    renderSkill,
    parseSkill,
    parseSkillLenient,
    skillGraph,
  )
where

import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.Char (isSpace)
import Data.Foldable (traverse_)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Skillx.Graph (Graph)
import Skillx.Graph qualified as Graph
import Skillx.Name (Name, NameError, mkName, nameText)

-- | One procedural skill node.
data Skill = Skill
  { -- | The identifier of the skill.
    skillName :: Name,
    -- | One-line summary and trigger description for agent routing.
    skillDescription :: Text,
    -- | Outgoing links to other skills in the store.
    skillLinks :: Set Name,
    -- | MCP servers required by this skill.
    skillRequiresMcp :: Set Name,
    -- | Lifecycle hooks required by this skill.
    skillRequiresHook :: Set Name,
    -- | CLI binaries required on @PATH@ by this skill.
    skillRequiresBin :: Set Name,
    -- | The Markdown body of the skill.
    skillBody :: Text
  }
  deriving (Eq, Show)

-- | Why 'mkSkill' rejects a field.
data SkillError
  = -- | The description is empty, spans multiple lines, or has leading or
    -- trailing whitespace.
    InvalidDescription Text
  deriving (Eq, Show)

-- | Why 'parseSkill' or 'parseSkillLenient' rejects a file.
data SkillParseError
  = -- | The file is not valid UTF-8.
    BadEncoding
  | -- | The file does not begin with @---\n@ and close its frontmatter with
    -- @\n---\n\n@.
    MissingFrontmatter
  | -- | A required header (@name@ or @description@) is missing.
    MissingHeader Text
  | -- | A header appears more than once in strict mode, or a single-valued
    -- header appears more than once.
    DuplicateHeader Text
  | -- | A header name is not one of the known header names.
    UnknownHeader Text
  | -- | A header line does not contain @": "@.
    MalformedHeader Text
  | -- | A header value is invalid.
    BadHeaderValue Text Text
  | -- | A 'Name' in @name@, @links@, @requires-mcp@, @requires-hook@ or
    -- @requires-bin@ is not valid.
    BadHeaderName Text NameError
  | -- | The file is a valid skill, but its bytes are not in the canonical form
    -- produced by 'renderSkill'.
    NotCanonical
  deriving (Eq, Show)

-- | Check whether a text is a valid one-line description.
validDescription :: Text -> Bool
validDescription v =
  not (T.null v)
    && not (T.any (`elem` ['\n', '\r']) v)
    && not (T.any isSpace (T.take 1 v))
    && not (T.any isSpace (T.takeEnd 1 v))

-- | Construct a validated 'Skill'.
mkSkill ::
  Name ->
  Text ->
  Set Name ->
  Set Name ->
  Set Name ->
  Set Name ->
  Text ->
  Either SkillError Skill
mkSkill name desc links mcp hooks bins body
  | not (validDescription desc) = Left (InvalidDescription desc)
  | otherwise =
      Right
        Skill
          { skillName = name,
            skillDescription = desc,
            skillLinks = links,
            skillRequiresMcp = mcp,
            skillRequiresHook = hooks,
            skillRequiresBin = bins,
            skillBody = body
          }

-- | Render a 'Skill' in its canonical @SKILL.md@ byte representation.
renderSkill :: Skill -> ByteString
renderSkill s =
  encodeUtf8 (T.unlines ("---" : headerLines ++ ["---"]) <> "\n" <> skillBody s)
  where
    headerLines =
      [ "name: " <> nameText (skillName s),
        "description: " <> skillDescription s
      ]
        ++ optSet "links" (skillLinks s)
        ++ optSet "requires-mcp" (skillRequiresMcp s)
        ++ optSet "requires-hook" (skillRequiresHook s)
        ++ optSet "requires-bin" (skillRequiresBin s)
    optSet key vals
      | Set.null vals = []
      | otherwise =
          [key <> ": " <> T.intercalate ", " (map nameText (Set.toAscList vals))]

-- | Known frontmatter header names.
knownHeaders :: Set Text
knownHeaders =
  Set.fromList
    [ "name",
      "description",
      "links",
      "requires-mcp",
      "requires-hook",
      "requires-bin"
    ]

-- | Split a file into its header block (inside @---@ fences) and Markdown body.
splitFrontmatter :: Text -> Either SkillParseError (Text, Text)
splitFrontmatter t =
  case T.stripPrefix "---\n" t of
    Nothing -> Left MissingFrontmatter
    Just afterOpen ->
      let (hdrs, rest) = T.breakOn "\n---\n\n" afterOpen
       in if T.null rest
            then Left MissingFrontmatter
            else Right (hdrs, T.drop 6 rest)

-- | Split a single @key: value@ header line.
splitHeader :: Text -> Either SkillParseError (Text, Text)
splitHeader line =
  let (k, rest) = T.breakOn ": " line
   in if T.null rest
        then Left (MalformedHeader line)
        else Right (k, T.drop 2 rest)

-- | Parse a comma-or-whitespace separated list of 'Name' values for a header.
parseNameSet :: Text -> [Text] -> Either SkillParseError (Set Name)
parseNameSet hdr rawLines = do
  let tokens = concatMap splitTokens rawLines
  if any T.null rawLines || (not (null rawLines) && null tokens)
    then Left (BadHeaderValue hdr "")
    else Set.fromList <$> traverse (first (BadHeaderName hdr) . mkName) tokens
  where
    splitTokens =
      filter (not . T.null) . T.split (\c -> isSpace c || c == ',')

-- | Parse a 'Skill' without enforcing canonical header ordering, so @skillx fmt@
-- can normalize hand-written skill files.
parseSkillLenient :: ByteString -> Either SkillParseError Skill
parseSkillLenient bs = do
  text <- first (const BadEncoding) (decodeUtf8' bs)
  (hdrBlock, body) <- splitFrontmatter text
  pairs <- traverse splitHeader (T.splitOn "\n" hdrBlock)
  traverse_ checkKnown (map fst pairs)
  let groups = Map.fromListWith (flip (++)) [(k, [v]) | (k, v) <- pairs]
  nameRaw <- single "name" groups
  descRaw <- single "description" groups
  name <- first (BadHeaderName "name") (mkName nameRaw)
  links <- parseNameSet "links" (Map.findWithDefault [] "links" groups)
  mcp <- parseNameSet "requires-mcp" (Map.findWithDefault [] "requires-mcp" groups)
  hooks <- parseNameSet "requires-hook" (Map.findWithDefault [] "requires-hook" groups)
  bins <- parseNameSet "requires-bin" (Map.findWithDefault [] "requires-bin" groups)
  first (\(InvalidDescription d) -> BadHeaderValue "description" d) $
    mkSkill name descRaw links mcp hooks bins body
  where
    checkKnown k
      | k `Set.member` knownHeaders = Right ()
      | otherwise = Left (UnknownHeader k)
    single k m = case Map.lookup k m of
      Just [x] -> Right x
      Just (_ : _ : _) -> Left (DuplicateHeader k)
      _ -> Left (MissingHeader k)

-- | Parse a 'Skill' in strict canonical form.
--
-- For every skill @s@ made with 'mkSkill',
-- @parseSkill (renderSkill s) == Right s@, and for every @bs@ with
-- @parseSkill bs == Right s@, @renderSkill s == bs@.
parseSkill :: ByteString -> Either SkillParseError Skill
parseSkill bs = do
  s <- parseSkillLenient bs
  if renderSkill s == bs
    then Right s
    else Left NotCanonical

-- | The one-node 'Graph' of a skill and its outgoing 'skillLinks'.
skillGraph :: Skill -> Graph
skillGraph s = Graph.singleton (skillName s) (skillLinks s)
