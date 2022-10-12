module Latuc where

import           Prelude

import           Control.Applicative    (liftA2)
import           Control.Category       ((>>>))
import           Control.Monad          (void)
import           Data.Char              (isLetter, isSpace)
import           Data.Foldable          (fold)
import           Data.Function          ((&))
import           Data.Functor           ((<&>))
import           Data.Functor.Identity  (Identity (..))
import           Data.List              (isPrefixOf)
import           Data.Maybe             (fromMaybe)
import qualified Data.Set               as Set
import           Text.Parsec            hiding (count)

import qualified Helper.Binary          as Binary
import qualified Helper.Escape          as Escape
import qualified Helper.Style           as Style
import qualified Helper.Unary           as Unary
import qualified Helper.UnaryWithOption as UnaryWithOption

type Parser = ParsecT String () Identity

fromEither :: Either String a -> Parser a
fromEither = \case
  Left err  -> fail err
  Right res -> pure res

singleton :: a -> [a]
singleton = (:[])

isLiteralChar :: Char -> Bool
isLiteralChar c =
  not (isSpace c) && (c `notElem` "$^-_~{}\\")


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
          if Set.member name Style.names
          then pure ""
          else handleCommand name

commandBlock :: Parser String
commandBlock = commandName >>= handleCommand

handleCommand :: String -> Parser String
handleCommand command = if
  | command `elem` Escape.names          -> handleEscapeChars command
  | command `elem` Unary.names           -> handleUnaries command
  | command `elem` Binary.names          -> handleBinaries command
  | command `elem` Style.names           -> handleStyles command
  | command `elem` UnaryWithOption.names -> handleUnaryWithOption command
  | otherwise                            -> handleUnknown command

  where

  handleEscapeChars :: String -> Parser String
  handleEscapeChars e =
    fromEither $ Escape.translate e

  handleUnaries :: String -> Parser String
  handleUnaries u = do
    whitespaceNoBreak
    p <- commandParam
    fromEither $ Unary.translate u p

  handleBinaries :: String -> Parser String
  handleBinaries b = do
    whitespaceNoBreak
    p1 <- commandParam
    whitespaceNoBreak
    p2 <- commandParam
    fromEither $ Binary.translate b p1 p2

  handleStyles :: String -> Parser String
  handleStyles s = do
    nl <- whitespaceCanBreak
    p <- blocks
    fromEither . fmap (nl <>) $ Style.translate s p

  handleUnaryWithOption :: String -> Parser String
  handleUnaryWithOption uo = do
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
    fromEither $ UnaryWithOption.translate uo (opt & fromMaybe "") p

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
