module Helper.Unary where

import           Control.Category ((>>>))
import           Data.Char        (isControl)
import           Data.Function    ((&))
import           Data.List.Extra  (trim)
import qualified Data.Map         as Map
import           Data.Maybe       (fromMaybe)
import           Data.Set         (Set)
import qualified Data.Set         as Set

import           Helper.Mappings  (CombiningType (..))
import qualified Helper.Mappings  as Mappings



isCombiningChar :: Char -> Bool
isCombiningChar char =
     '\x0300' <= char && char <= '\x036F'
  || '\x1AB0' <= char && char <= '\x1AFF'
  || '\x1DC0' <= char && char <= '\x1DFF'
  || '\x20D0' <= char && char <= '\x20FF'
  || '\xFE20' <= char && char <= '\xFE20'

isCombiningOrControlChar :: Char -> Bool
isCombiningOrControlChar char =
  isCombiningChar char || isControl char
    -- nb. Not sure if 'isControl' is the same as Mappings.scala 'Character.isISOControl'

isCombiningCommand :: String -> Bool
isCombiningCommand = flip Map.member Mappings.combining

translateCombining :: String -> String -> Either String String
translateCombining command str =
  case Map.lookup command Mappings.combining of
    Nothing -> Left $ "Unknown combining command: " <> command
    Just (combiningChar, combiningType) -> Right $
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


makeNot :: String -> String
makeNot negated =
  let s = case trim negated of
            ""      -> " "
            trimmed -> trimmed
  in case Map.lookup s Mappings.not of
            Just r  -> r
            Nothing -> [head s] <> "\x0338" <> tail s


tryMakeSubscript :: String -> Maybe String
tryMakeSubscript str =
  if null str then Just ""
  else traverse (flip Map.lookup Mappings.subscripts) str

makeSubscript :: String -> String
makeSubscript str =
  case trim str of
    "" -> ""
    s  -> tryMakeSubscript s & fromMaybe ("_(" <> s <> ")")

tryMakeSuperscript :: String -> Maybe String
tryMakeSuperscript str =
  case str of
    "" -> Just ""
    s  -> traverse (flip Map.lookup Mappings.superscripts) s

makeSuperscript :: String -> String
makeSuperscript str =
  case trim str of
    "" -> ""
    s  -> tryMakeSuperscript s & fromMaybe ("^(" <> s <> ")")


isStylesCommand :: String -> Bool
isStylesCommand = flip Map.member Mappings.styles

translateStyles :: String -> String -> Either String String
translateStyles command str =
  case Map.lookup command Mappings.styles of
    Nothing -> Left $ "Unknown styles command: " <> command
    Just mp -> Right $ str >>= (\c -> Map.lookup c mp & fromMaybe [c])

names :: Set String
names = Set.fromList
  [ "\\not"
  , "_"
  , "^"
  , "\\textsubscript"
  , "\\textsuperscript"
  ]
  <> (Set.fromList $ Map.keys Mappings.combining)
  <> (Set.fromList $ Map.keys Mappings.styles)

translate :: String -> String -> Either String String
translate command param =
  if
    | Prelude.not (Set.member command names) -> Left $ "Unknown command: " <> command
    | command `elem` ["_", "\\textsubscript"] -> Right $ makeSubscript param
    | command `elem` ["^", "\\textsuperscript"] -> Right $ makeSuperscript param
    | command == "\\not" -> Right $ makeNot param
    | isCombiningCommand command -> translateCombining command param
    | isStylesCommand command -> translateStyles command param
    | otherwise -> error "missed case in Unary.translate"

