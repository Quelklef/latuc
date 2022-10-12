module Helper.Binary where

import qualified Helper.Unary    as Unary

import           Data.Char       (GeneralCategory (ConnectorPunctuation, OtherNumber),
                                  generalCategory, isDigit, isLetter)
import           Data.List.Extra (trim)
import           Data.Map        (Map)
import qualified Data.Map        as Map
import           Data.Set        (Set)
import qualified Data.Set        as Set


frac :: Map (String, String) String
frac = Map.fromList
  [ (("1", "2"), "½")
  , (("1", "3"), "⅓")
  , (("2", "3"), "⅔")
  , (("1", "4"), "¼")
  , (("3", "4"), "¾")
  , (("1", "5"), "⅕")
  , (("2", "5"), "⅖")
  , (("3", "5"), "⅗")
  , (("4", "5"), "⅘")
  , (("1", "6"), "⅙")
  , (("5", "6"), "⅚")
  , (("1", "8"), "⅛")
  , (("3", "8"), "⅜")
  , (("5", "8"), "⅝")
  , (("7", "8"), "⅞")
  ]

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
  else case Map.lookup (num, den) frac of
          Just s -> s
          Nothing -> "(" <> maybeParenthesize num <> "/" <> maybeParenthesize den <> ")"

names :: Set String
names = Set.fromList ["\\frac"]

translate :: String -> String -> String -> Either String String
translate command param1 param2 =
  case Set.member command names of
    False -> Left $ "Unknown command: " <> command
    True  -> Right $ makeFraction param1 param2
