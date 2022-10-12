-- package com.github.tomtung.latex2unicode
--
-- object LaTeX2Unicode {
--   import fastparse._
--   import fastparse.NoWhitespace._

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


--   private[this] def isLiteralChar(c: Char): Boolean =
--     !c.isWhitespace && "$^-_~{}\\".indexOf(c) == -1
isLiteralChar :: Char -> Bool
isLiteralChar c =
  not (isSpace c) && (c `notElem` "$^-_~{}\\")


--   private[this] def spacesCountNewLines[_: P]: P[Int] =
--     P(CharsWhile(_.isWhitespace).! ~/ Pass).map(_.count(_ == '\n'))
spacesCountNewLines :: Parser Int
spacesCountNewLines =
  cnt (== '\n') <$> many1 space
  where cnt p = length . filter p


--   private[this] def spacesBlock[_: P]: P[String] =
--     spacesCountNewLines.map(cnt => {
--       if (cnt <= 1) " "
--       else "\n\n"
--     })
spacesBlock :: Parser String
spacesBlock = do
  n <- spacesCountNewLines
  pure $ if n <= 1 then " " else "\n\n"


--   private[this] def literalCharsBlock[_: P]: P[String] = P(
--     CharsWhile(isLiteralChar) ~/ Pass
--   ).!
literalCharsBlock :: Parser String
literalCharsBlock = many1 (satisfy isLiteralChar)


--   private[this] def bracketBlock[_: P]: P[String] = P(
--     "{" ~/ blocks ~ "}" ~/ Pass
--   )
bracketBlock :: Parser String
bracketBlock = string "{" *> blocks <* string "}"


--   private[this] object command {

--     private def ignoreSpaces[_: P]: P[Unit] = P(spacesCountNewLines.?).flatMap({
--       case None => Pass
--       case Some(cnt) =>
--         if (cnt <= 1) Pass
--         else Fail
--     })
command_ignoreSpaces :: Parser ()
command_ignoreSpaces = do
  try (optionMaybe spacesCountNewLines >>= \case
          Nothing -> pure ()
          Just cnt -> if
            | cnt <= 1  -> pure ()
            | otherwise -> fail "!")
  <|> pure ()


--     private def PassWithEmptyString[_: P]: P[String] = Pass("")
command_PassWithEmptyString :: Parser String
command_PassWithEmptyString = pure ""


--     private def PassWithNewLine[_: P]: P[String] = Pass("\n\n")
command_PassWithNewLine :: Parser String
command_PassWithNewLine = pure "\n\n"


--     private def maybeNewLine[_: P]: P[String] =
--       P(spacesCountNewLines.?).flatMap({
--         case None => PassWithEmptyString
--         case Some(cnt) =>
--           if (cnt <= 1) PassWithEmptyString
--           else PassWithNewLine
--       })
command_maybeNewLine :: Parser String
command_maybeNewLine =
  try (spacesCountNewLines >>= (\cnt -> if cnt <= 1 then command_PassWithEmptyString else command_PassWithNewLine))
  <|> command_PassWithEmptyString


--     private def param[_: P]: P[String] = P(
--       bracketBlock | command.commandBlock | P(CharPred(isLiteralChar).!)
--     )
command_param :: Parser String
command_param = try (bracketBlock <|> command_commandBlock) <|> (singleton <$> satisfy isLiteralChar)


--     def name[_: P]: P[String] = P(
--       (("-".rep(1) | CharIn("$^_~")).! ~/ Pass) |
--         ("\\" ~/ (CharsWhile(_.isLetter) | AnyChar) ~/ Pass).!
--     )
command_name :: Parser String
command_name =
  try ((fmap fold . many1) (string "-"))
  <|> try ((fmap singleton . satisfy) (`elem` "$^_~"))
  <|> liftA2 (<>) (string "\\") (try (many1 $ satisfy isLetter) <|> fmap singleton anyChar)


--     // Literals inside option must not contain "]"
--     private def literalCharsBlockInOption[_: P]: P[String] = P(
--       CharsWhile(c => c != ']' && isLiteralChar(c))
--     ).!
command_literalCharsBlockInOption :: Parser String
command_literalCharsBlockInOption = (many1 . satisfy) (\c -> c /= ']' && isLiteralChar c)


--     private def commandBlockInOption[_: P] = P(name.flatMap(s => {
--       // Ignoring styles in command option is just for simplicity
--       if (helper.Style.names.contains(s)) PassWithEmptyString
--       else handleCommand.apply(s)
--     }))
command_commandBlockInOption :: Parser String
command_commandBlockInOption =
  command_name >>=
    (\s -> if Set.member s Style.names
           then command_PassWithEmptyString
           else command_handleCommand s)


--     private def blockInOption[_: P]: P[String] = P(
--       literalCharsBlockInOption | bracketBlock | commandBlockInOption
--     )
command_blockInOption :: Parser String
command_blockInOption =
  try command_literalCharsBlockInOption <|> try bracketBlock <|> command_commandBlockInOption


--     private def blocksInOption[_: P]: P[String] =
--       P(blockInOption.rep).map(_.mkString)
command_blocksInOption :: Parser String
command_blocksInOption = fold <$> many command_blockInOption


--     def handleEscapeChars[_: P]: PartialFunction[String, P[String]] = {
--       case e if helper.Escape.names.contains(e) =>
--         Pass.map(_ => helper.Escape.translate(e))
--     }
command_handleEscapeChars :: String -> Parser String
command_handleEscapeChars e =
  case Escape.translate e of
    Left err  -> fail err
    Right res -> pure res


--     def handleUnaries[_: P]: PartialFunction[String, P[String]] = {
--       case u if helper.Unary.names.contains(u) =>
--         P(ignoreSpaces ~ param).map(p => helper.Unary.translate(u, p))
--     }
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


--     def handleBinaries[_: P]: PartialFunction[String, P[String]] = {
--       case b if helper.Binary.names.contains(b) =>
--         P(ignoreSpaces ~ param ~ ignoreSpaces ~ param).map({ case (p1, p2) =>
--           helper.Binary.translate(b, p1, p2)
--         })
--     }
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


--     def handleStyles[_: P]: PartialFunction[String, P[String]] = {
--       case s if helper.Style.names.contains(s) =>
--         P(maybeNewLine ~ blocks).map({ case (nl, p) =>
--           nl + helper.Style.translate(s, p)
--         })
--     }
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


--     def handleUnaryWithOption[_: P]: PartialFunction[String, P[String]] = {
--       case uo if helper.UnaryWithOption.names.contains(uo) =>
--         P(
--           ignoreSpaces ~ ("[" ~/ ignoreSpaces ~ blocksInOption ~ ignoreSpaces ~ "]").? ~/
--             ignoreSpaces ~ param
--         ).map({ case (opt, p) =>
--           helper.UnaryWithOption.translate(uo, opt.getOrElse(""), p)
--         })
--     }
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


--     def handleUnknown[_: P]: PartialFunction[String, P[String]] = {
--       case other => unknownCommand(other)
--     }
command_handleUnknown :: String -> Parser String
command_handleUnknown other =
  unknownCommand other


--     def handleCommand[_: P]: PartialFunction[String, P[String]] =
--       handleEscapeChars
--         .orElse(handleUnaries)
--         .orElse(handleBinaries)
--         .orElse(handleStyles)
--         .orElse(handleUnaryWithOption)
--         .orElse(handleUnknown)
command_handleCommand :: String -> Parser String
command_handleCommand command = if
  | command `elem` Escape.names          -> command_handleEscapeChars command
  | command `elem` Unary.names           -> command_handleUnaries command
  | command `elem` Binary.names          -> command_handleBinaries command
  | command `elem` Style.names           -> command_handleStyles command
  | command `elem` UnaryWithOption.names -> command_handleUnaryWithOption command
  | otherwise                            -> command_handleUnknown command


--     def commandBlock[_: P]: P[String] = name.flatMap(handleCommand)
command_commandBlock :: Parser String
command_commandBlock = command_name >>= command_handleCommand


--     def unknownCommand[_: P](command: String): P[String] = {
--       if (!command.startsWith("\\")) {
--         // Is not a command in the strong sense, so just return
--         return Pass(command) // PassWith(command)
--       }
--
--       val parserNoParam = () => Pass(command)
--       val parserUnary = () => P(param).map(p => command + p)
--       val parserBinary = () =>
--         P(param ~ param).map({ case (p1, p2) =>
--           s"$command{$p1}{$p2}"
--         })
--       val parserTernary = () =>
--         P(param ~ param ~ param).map({ case (p1, p2, p3) =>
--           s"$command{$p1}{$p2}{$p3}"
--         })
--
--       P(parserTernary() | parserBinary() | parserUnary() | parserNoParam())
--     }
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


--   }

--   private[this] def block[_: P]: P[String] = P(
--     spacesBlock | literalCharsBlock | bracketBlock | command.commandBlock
--   )
block :: Parser String
block = spacesBlock <|> literalCharsBlock <|> bracketBlock <|> command_commandBlock


--   private[this] def blocks[_: P]: P[String] = P(block.rep).map(_.mkString)
blocks :: Parser String
blocks = fold <$> many block


--   private def input[_: P]: P[String] = P(blocks ~ End)
input :: Parser String
input = blocks <* eof


runIt :: Parser String -> (String -> Either ParseError String)
runIt parser = runParser parser () "<input>"


--   /** Parse and try to convert LaTeX markup to Unicode.
--     * @param latex
--     *   LaTeX markup
--     * @return
--     *   a fastparse Parsed object that contains parsing result information.
--     */
--   def parse(latex: String): Parsed[String] =
--     fastparse.parse[String](latex, input(_))
parse :: String -> Either ParseError String
parse = runIt input


--   def parseBlock(latex: String): Parsed[String] =
--     fastparse.parse[String](latex, block(_))
parseBlock :: String -> Either ParseError String
parseBlock = runIt block


--   def parseBlocks(latex: String): Parsed[String] =
--     fastparse.parse[String](latex, blocks(_))
parseBlocks :: String -> Either ParseError String
parseBlocks = runIt blocks


--   /** Converts LaTeX markup to Unicode whenever possible. <br /> When parse
--     * fails, simply fallback to the original input string.
--     * @param latex
--     *   LaTeX markup
--     * @return
--     *   Resultant Unicode string
--     */
--   def convert(latex: String): String = try {
--     this.parse(latex) match {
--       case Parsed.Success(result, _) =>
--         result
--
--       // If parsing fails, just return the original string
--       case Parsed.Failure(l, _, _) =>
--         latex
--     }
--   } catch {
--     // If anything bad happens, just return the original string
--     case e: Throwable => latex
--   }
-- }
convert :: String -> String
convert latex =
  case Latuc.parse latex of
    Left _    -> latex
    Right res -> res
