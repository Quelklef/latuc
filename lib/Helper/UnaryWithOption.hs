module Helper.UnaryWithOption where

import qualified Helper.Unary    as Unary

import           Data.Either     (fromRight)
import           Data.Function   ((&))
import           Data.List.Extra (trim)
import           Data.Maybe      (fromMaybe)
import           Data.Set        (Set)
import qualified Data.Set        as Set


makeSqrt :: String -> String -> String
makeSqrt index radicand =
  let radix = case index of
        "" -> "√"
        "2" -> "√"
        "3" -> "∛"
        "4" -> "∜"
        _ -> (<> "√") (Unary.tryMakeSuperscript index & fromMaybe ("(" <> index <> ")"))

  in radix <> (fromRight impossible $ Unary.translateCombining "\\overline" radicand)

  where impossible = error "impossible"

names :: Set String
names = Set.fromList ["\\sqrt"]

translate :: String -> String -> String -> Either String String
translate command option param =
  if command /= "\\sqrt"
  then Left $ "Unknown command: " <> command
  else Right $ makeSqrt (trim option) (trim param)
