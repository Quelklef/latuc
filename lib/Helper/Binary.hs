module Helper.Binary where

import qualified Helper.Unary    as Unary

import           Data.Char       (GeneralCategory (ConnectorPunctuation, OtherNumber),
                                  generalCategory, isDigit, isLetter)
import           Data.List.Extra (trim)
import qualified Data.Map        as Map
import           Data.Set        (Set)
import qualified Data.Set        as Set

import qualified Helper.Mappings as Mappings


shouldParenthesizeStringWithChar :: Char -> Bool
shouldParenthesizeStringWithChar c =
  not (isLetter c || isDigit c)
  && not (Unary.isCombiningChar c)
  && generalCategory c /= OtherNumber
  && generalCategory c /= ConnectorPunctuation

maybeParenthesize :: String -> String
maybeParenthesize s =
  if any shouldParenthesizeStringWithChar s
  then "(" <> s <> ")"
  else s

makeFraction :: String -> String -> String
makeFraction (trim -> num) (trim -> den) =
  if (num, den) == ("", "") then ""
  else case Map.lookup (num, den) Mappings.frac of
          Just s -> s
          Nothing -> "(" <> maybeParenthesize num <> "/" <> maybeParenthesize den <> ")"

names :: Set String
names = Set.fromList ["\\frac"]

translate :: String -> String -> String -> Either String String
translate command param1 param2 =
  case Set.member command names of
    False -> Left $ "Unknown command: " <> command
    True  -> Right $ makeFraction param1 param2
