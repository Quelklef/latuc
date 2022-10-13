module Latuc where

import           Prelude

import           Control.Applicative   (liftA2)
import           Control.Category      ((>>>))
import           Control.Monad         (void)
import           Data.Char             (GeneralCategory (ConnectorPunctuation, OtherNumber),
                                        generalCategory, isControl, isDigit,
                                        isLetter, isSpace)
import           Data.Foldable         (fold)
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Functor.Identity (Identity (..))
import           Data.List             (isPrefixOf)
import           Data.List.Extra       (trim)
import           Data.Map.Strict       (Map)
import qualified Data.Map.Strict       as Map
import           Data.Maybe            (fromMaybe)
import           Text.Parsec           hiding (count)

import           Latuc.Mappings        (CombiningType (..))
import qualified Latuc.Mappings        as Mappings


type Parser = ParsecT String () Identity

fromEither :: Either String a -> Parser a
fromEither = \case
  Left err  -> fail err
  Right res -> pure res

singleton :: a -> [a]
singleton = (:[])

isLiteralChar :: Char -> Bool
isLiteralChar c =
  not (isSpace c) && (c `notElem` "$^-_~{}\\")  -- WANT: this is uncollated knowledge

isCombiningChar :: Char -> Bool
isCombiningChar ch =
     '\x0300' <= ch && ch <= '\x036F'
  || '\x1AB0' <= ch && ch <= '\x1AFF'
  || '\x1DC0' <= ch && ch <= '\x1DFF'
  || '\x20D0' <= ch && ch <= '\x20FF'
  || '\xFE20' <= ch && ch <= '\xFE20'

isCombiningOrControlChar :: Char -> Bool
isCombiningOrControlChar ch =
  isCombiningChar ch || isControl ch
    -- nb. Not sure if 'isControl' is the same as Mappings.scala 'Character.isISOControl'


data HadBreak = HadBreak | DidntHaveBreak

whitespace1 :: Parser HadBreak
whitespace1 = many1 space <&> (count (== '\n') >>> (> 1) >>> fromBool)
  where

  count p = length . filter p

  fromBool True  = HadBreak
  fromBool False = DidntHaveBreak

whitespaceNoBreak :: Parser ()
whitespaceNoBreak = do
  optionMaybe whitespace1
  >>= \case
    Nothing             -> pure ()
    Just DidntHaveBreak -> pure ()
    Just HadBreak       -> fail ""

whitespaceCanBreak :: Parser String
whitespaceCanBreak =
  optionMaybe whitespace1
  <&> \case
    Nothing             -> ""
    Just DidntHaveBreak -> ""
    Just HadBreak       -> "\n\n"

spacesBlock :: Parser String
spacesBlock =
  whitespace1
  <&> \case
    HadBreak       -> "\n\n"
    DidntHaveBreak -> " "

literalCharsBlock :: Parser String
literalCharsBlock = many1 (satisfy isLiteralChar)

bracketBlock :: Parser String
bracketBlock = string "{" *> blocks <* string "}"

commandParam :: Parser String
commandParam = try (bracketBlock <|> commandBlock) <|> (singleton <$> satisfy isLiteralChar)

commandName :: Parser String
commandName =
  try ((fmap fold . many1) (string "-"))
  <|> try ((fmap singleton . satisfy) (`elem` "$^_~"))
  <|> liftA2 (<>) (string "\\") (try (many1 $ satisfy isLetter) <|> fmap singleton anyChar)

commandLiteralCharsBlockInOption :: Parser String
commandLiteralCharsBlockInOption = (many1 . satisfy) (\c -> c /= ']' && isLiteralChar c)

commandBlockInOption :: Parser String
commandBlockInOption =
  try commandLiteralCharsBlockInOption
  <|> try bracketBlock
  <|> do
          name <- commandName
          if Map.member name Mappings.forwardStyles  -- WANT: this is uncollated knowledge
          then pure ""
          else handleCommand name

commandBlock :: Parser String
commandBlock = commandName >>= handleCommand

handleCommand :: String -> Parser String
handleCommand command =
  case Map.lookup command commands of
    Just parser -> parser
    Nothing     -> handleUnknown command

  where

  handleUnknown :: String -> Parser String
  handleUnknown other =
    if not ("\\" `isPrefixOf` other)
    then pure other
    else let
      parserNoParam = pure other
      parserUnary = do
        p1 <- commandParam
        pure $ other <> "{" <> p1 <> "}"
      parserBinary = do
        p1 <- commandParam
        p2 <- commandParam
        pure $ other <> "{" <> p1 <> "}{" <> p2 <> "}"
      parserTernary = do
        p1 <- commandParam
        p2 <- commandParam
        p3 <- commandParam
        pure $ other <> "{" <> p1 <> "}{" <> p2 <> "}{" <> p3 <> "}"

      in try parserTernary <|> try parserBinary <|> try parserUnary <|> parserNoParam

block :: Parser String
block = spacesBlock <|> literalCharsBlock <|> bracketBlock <|> commandBlock

blocks :: Parser String
blocks = fold <$> many block


commands :: Map String (Parser String)
commands =

  fold
    [ cConstants
    , cCombining
    , cStyles
    , cNot
    , cSubSupScr
    , cFrac
    , cSqrt
    ]

  where

  -- nb. WANT: length-polymorphic "parse args" function using typelevel nats

  mkConstant :: String -> Parser String
  mkConstant = pure

  mkUnary :: (String -> String) -> Parser String
  mkUnary fun = do
    whitespaceNoBreak
    param <- commandParam
    pure $ fun param

  mkUnaryWithOption :: (String -> String -> String) -> Parser String
  mkUnaryWithOption fun = do
    whitespaceNoBreak
    opt <- optionMaybe (do
      void $ string "["
      whitespaceNoBreak
      c <- fold <$> many commandBlockInOption
      whitespaceNoBreak
      void $ string "]"
      pure c)
    whitespaceNoBreak
    p <- commandParam
    pure $ fun (opt & fromMaybe "") p


  mkBinary :: (String -> String -> String) -> Parser String
  mkBinary fun = do
    whitespaceNoBreak
    param1 <- commandParam
    whitespaceNoBreak
    param2 <- commandParam
    pure $ fun param1 param2

  cConstants :: Map String (Parser String)
  cConstants = mkConstant <$> Mappings.constants

  cCombining :: Map String (Parser String)
  cCombining =
    (mkUnary . uncurry applyCombining) <$> Mappings.combining

  cStyles :: Map String (Parser String)
  cStyles =
    fold
      [ (mkUnary . applyStyle) <$> Mappings.styles
      , (mkForwardUnary . applyStyle) <$> Mappings.forwardStyles
      ]

    where

    applyStyle :: Map Char String -> String -> String
    applyStyle style = (>>= (\c -> Map.lookup c style & fromMaybe [c]))

    mkForwardUnary :: (String -> String) -> Parser String
    mkForwardUnary fun = do
      nl <- whitespaceCanBreak
      param <- blocks
      pure . (nl <>) $ fun param

  cNot :: Map String (Parser String)
  cNot = Map.singleton "\\not" (mkUnary notImpl)
    where

    notImpl :: String -> String
    notImpl = trim >>> \case
        "" -> " \x0338"
        str@(s:ss) ->
          Map.lookup str Mappings.not
          & fromMaybe ([s] <> "\x0338" <> ss)

  cSubSupScr :: Map String (Parser String)
  cSubSupScr =
    Map.fromList
      [ ("_"                , mkUnary subscrImpl)
      , ("\\textsubscript"  , mkUnary subscrImpl)
      , ("^"                , mkUnary supscrImpl)
      , ("\\textsuperscript", mkUnary supscrImpl)
      ]

    where

    subscrImpl :: String -> String
    subscrImpl (trim -> str) =
      str
      & traverse (flip Map.lookup Mappings.subscript)
      & fromMaybe ("_(" <> str <> ")")

    supscrImpl :: String -> String
    supscrImpl (trim -> str) =
      str
      & traverse (flip Map.lookup Mappings.supscript)
      & fromMaybe ("^(" <> str <> ")")

  cFrac :: Map String (Parser String)
  cFrac = Map.singleton "\\frac" (mkBinary fracImpl)
    where

    shouldParenthesizeStringWithChar :: Char -> Bool
    shouldParenthesizeStringWithChar c =
      not (isLetter c || isDigit c)
      && not (isCombiningChar c)
      && generalCategory c /= OtherNumber
      && generalCategory c /= ConnectorPunctuation

    maybeParenthesize :: String -> String
    maybeParenthesize s =
      if any shouldParenthesizeStringWithChar s
      then "(" <> s <> ")"
      else s

    fracImpl :: String -> String -> String
    fracImpl (trim -> num) (trim -> den) =
      if (num, den) == ("", "") then ""
      else case Map.lookup (num, den) Mappings.frac of
              Just s -> s
              Nothing -> "(" <> maybeParenthesize num <> "/" <> maybeParenthesize den <> ")"

  cSqrt :: Map String (Parser String)
  cSqrt = Map.singleton "\\sqrt" (mkUnaryWithOption sqrtImpl)
    where

    sqrtImpl :: String -> String -> String
    sqrtImpl = makeSqrt

    makeSqrt :: String -> String -> String
    makeSqrt index radicand =
      let radix = case index of
            "" -> "√"
            "2" -> "√"
            "3" -> "∛"
            "4" -> "∜"
            _ -> (<> "√") $ traverse (flip Map.lookup Mappings.supscript) index
                            & fromMaybe ("(" <> index <> ")")

      in radix <> applyCombining '\x0305' EveryChar radicand


applyCombining :: Char -> CombiningType -> (String -> String)
applyCombining combiningChar combiningType str =
  let strOrSpace = if str == "" then " " else str
  in case combiningType of
    FirstChar ->
      let i = (+ 1) . length . takeWhile isCombiningOrControlChar . drop 1 $ strOrSpace
      in take i strOrSpace <> [combiningChar] <> drop i strOrSpace
    LastChar ->
      strOrSpace <> [combiningChar]
    EveryChar ->
      if str == "" then ""
      else let
        result = go str
        go "" = ""
        go s =
          let tk = take 1 <> (drop 1 >>> takeWhile isCombiningOrControlChar)
              dp = drop 1 >>> dropWhile isCombiningOrControlChar
              in tk s <> [combiningChar] <> go (dp s)
        in result


runIt :: Parser String -> (String -> Either ParseError String)
runIt parser = runParser parser () "<input>"

parse :: String -> Either ParseError String
parse = runIt (blocks <* eof)

parseBlock :: String -> Either ParseError String
parseBlock = runIt block

parseBlocks :: String -> Either ParseError String
parseBlocks = runIt blocks

convert :: String -> String
convert latex =
  case Latuc.parse latex of
    Left _    -> latex
    Right res -> res
