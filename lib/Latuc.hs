module Latuc where

import           Control.Applicative    (liftA2)
import           Control.Monad          (void)
import           Data.Char              (isLetter, isSpace)
import           Data.Foldable          (fold)
import           Data.Function          ((&))
import           Data.Functor.Identity  (Identity (..))
import           Data.List              (isPrefixOf)
import           Data.Maybe             (fromMaybe)
import qualified Data.Set               as Set
import           Text.Parsec

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


spacesCountNewLines :: Parser Int
spacesCountNewLines =
  cnt (== '\n') <$> many1 space
  where cnt p = length . filter p

spacesBlock :: Parser String
spacesBlock = do
  n <- spacesCountNewLines
  pure $ if n <= 1 then " " else "\n\n"

literalCharsBlock :: Parser String
literalCharsBlock = many1 (satisfy isLiteralChar)

bracketBlock :: Parser String
bracketBlock = string "{" *> blocks <* string "}"

command_ignoreSpaces :: Parser ()
command_ignoreSpaces = do
  optionMaybe spacesCountNewLines >>= \case
          Nothing -> pure ()
          Just cnt -> if
            | cnt <= 1  -> pure ()
            | otherwise -> fail "!"

command_maybeNewLine :: Parser String
command_maybeNewLine =
  try (spacesCountNewLines >>= pure . (\cnt -> if cnt <= 1 then "" else "\n\n"))
  <|> pure ""

command_param :: Parser String
command_param = try (bracketBlock <|> command_commandBlock) <|> (singleton <$> satisfy isLiteralChar)

command_name :: Parser String
command_name =
  try ((fmap fold . many1) (string "-"))
  <|> try ((fmap singleton . satisfy) (`elem` "$^_~"))
  <|> liftA2 (<>) (string "\\") (try (many1 $ satisfy isLetter) <|> fmap singleton anyChar)

command_literalCharsBlockInOption :: Parser String
command_literalCharsBlockInOption = (many1 . satisfy) (\c -> c /= ']' && isLiteralChar c)

command_commandBlockInOption :: Parser String
command_commandBlockInOption =
  command_name >>=
    (\s -> if Set.member s Style.names
           then pure ""
           else command_handleCommand s)

command_blockInOption :: Parser String
command_blockInOption =
  try command_literalCharsBlockInOption <|> try bracketBlock <|> command_commandBlockInOption

command_commandBlock :: Parser String
command_commandBlock = command_name >>= command_handleCommand

command_handleCommand :: String -> Parser String
command_handleCommand command = if
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
    command_ignoreSpaces
    p <- command_param
    fromEither $ Unary.translate u p

  handleBinaries :: String -> Parser String
  handleBinaries b = do
    command_ignoreSpaces
    p1 <- command_param
    command_ignoreSpaces
    p2 <- command_param
    fromEither $ Binary.translate b p1 p2

  handleStyles :: String -> Parser String
  handleStyles s = do
    nl <- command_maybeNewLine
    p <- blocks
    fromEither . fmap (nl <>) $ Style.translate s p

  handleUnaryWithOption :: String -> Parser String
  handleUnaryWithOption uo = do
    command_ignoreSpaces
    opt <- optionMaybe (do
      void $ string "["
      command_ignoreSpaces
      c <- fold <$> many command_blockInOption
      command_ignoreSpaces
      void $ string "]"
      pure c)
    command_ignoreSpaces
    p <- command_param
    fromEither $ UnaryWithOption.translate uo (opt & fromMaybe "") p

  handleUnknown :: String -> Parser String
  handleUnknown other =
    if not ("\\" `isPrefixOf` other)
    then pure other
    else let
      parserNoParam = pure other
      parserUnary = do
        p1 <- command_param
        pure $ other <> "{" <> p1 <> "}"
      parserBinary = do
        p1 <- command_param
        p2 <- command_param
        pure $ other <> "{" <> p1 <> "}{" <> p2 <> "}"
      parserTernary = do
        p1 <- command_param
        p2 <- command_param
        p3 <- command_param
        pure $ other <> "{" <> p1 <> "}{" <> p2 <> "}{" <> p3 <> "}"

      in try parserTernary <|> try parserBinary <|> try parserUnary <|> parserNoParam


block :: Parser String
block = spacesBlock <|> literalCharsBlock <|> bracketBlock <|> command_commandBlock

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
