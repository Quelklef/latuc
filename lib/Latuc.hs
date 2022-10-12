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
  try (optionMaybe spacesCountNewLines >>= \case
          Nothing -> pure ()
          Just cnt -> if
            | cnt <= 1  -> pure ()
            | otherwise -> fail "!")
  <|> pure ()


command_PassWithEmptyString :: Parser String
command_PassWithEmptyString = pure ""


command_PassWithNewLine :: Parser String
command_PassWithNewLine = pure "\n\n"


command_maybeNewLine :: Parser String
command_maybeNewLine =
  try (spacesCountNewLines >>= (\cnt -> if cnt <= 1 then command_PassWithEmptyString else command_PassWithNewLine))
  <|> command_PassWithEmptyString


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
           then command_PassWithEmptyString
           else command_handleCommand s)


command_blockInOption :: Parser String
command_blockInOption =
  try command_literalCharsBlockInOption <|> try bracketBlock <|> command_commandBlockInOption


command_blocksInOption :: Parser String
command_blocksInOption = fold <$> many command_blockInOption


command_handleEscapeChars :: String -> Parser String
command_handleEscapeChars e =
  case Escape.translate e of
    Left err  -> fail err
    Right res -> pure res


command_handleUnaries :: String -> Parser String
command_handleUnaries u =
  if not (Set.member u Unary.names)
  then fail "!"
  else do
    command_ignoreSpaces
    p <- command_param
    case Unary.translate u p of
      Left err  -> fail err
      Right res -> pure res


command_handleBinaries :: String -> Parser String
command_handleBinaries b =
  if not (Set.member b Binary.names)
  then fail "!"
  else do
    command_ignoreSpaces
    p1 <- command_param
    command_ignoreSpaces
    p2 <- command_param
    case Binary.translate b p1 p2 of
      Left err  -> fail err
      Right res -> pure res


command_handleStyles :: String -> Parser String
command_handleStyles s =
  if not (Set.member s Style.names)
  then fail "!"
  else do
    nl <- command_maybeNewLine
    p <- blocks
    case Style.translate s p of
      Left err  -> fail err
      Right res -> pure $ nl <> res


command_handleUnaryWithOption :: String -> Parser String
command_handleUnaryWithOption uo =
  if not (Set.member uo UnaryWithOption.names)
  then fail "!"
  else do
    command_ignoreSpaces
    opt <- optionMaybe (do
      void $ string "["
      command_ignoreSpaces
      c <- command_blocksInOption
      command_ignoreSpaces
      void $ string "]"
      pure c)
    command_ignoreSpaces
    p <- command_param
    case UnaryWithOption.translate uo (opt & fromMaybe "") p of
      Left err  -> fail err
      Right res -> pure res


command_handleUnknown :: String -> Parser String
command_handleUnknown other =
  unknownCommand other


command_handleCommand :: String -> Parser String
command_handleCommand command = if
  | command `elem` Escape.names          -> command_handleEscapeChars command
  | command `elem` Unary.names           -> command_handleUnaries command
  | command `elem` Binary.names          -> command_handleBinaries command
  | command `elem` Style.names           -> command_handleStyles command
  | command `elem` UnaryWithOption.names -> command_handleUnaryWithOption command
  | otherwise                            -> command_handleUnknown command


command_commandBlock :: Parser String
command_commandBlock = command_name >>= command_handleCommand


unknownCommand :: String -> Parser String
unknownCommand command =
  if not ("\\" `isPrefixOf` command)
  then pure command
  else let
    parserNoParam = pure command
    parserUnary = do
      p1 <- command_param
      pure $ command <> "{" <> p1 <> "}"
    parserBinary = do
      p1 <- command_param
      p2 <- command_param
      pure $ command <> "{" <> p1 <> "}{" <> p2 <> "}"
    parserTernary = do
      p1 <- command_param
      p2 <- command_param
      p3 <- command_param
      pure $ command <> "{" <> p1 <> "}{" <> p2 <> "}{" <> p3 <> "}"

    in try parserTernary <|> try parserBinary <|> try parserUnary <|> parserNoParam


block :: Parser String
block = spacesBlock <|> literalCharsBlock <|> bracketBlock <|> command_commandBlock


blocks :: Parser String
blocks = fold <$> many block


input :: Parser String
input = blocks <* eof


runIt :: Parser String -> (String -> Either ParseError String)
runIt parser = runParser parser () "<input>"


parse :: String -> Either ParseError String
parse = runIt input


parseBlock :: String -> Either ParseError String
parseBlock = runIt block


parseBlocks :: String -> Either ParseError String
parseBlocks = runIt blocks


convert :: String -> String
convert latex =
  case Latuc.parse latex of
    Left _    -> latex
    Right res -> res
