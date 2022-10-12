module Helper.Style where

import           Data.Map     (Map)
import qualified Data.Map     as Map
import           Data.Set     (Set)
import qualified Data.Set     as Set

import qualified Helper.Unary as Unary


alias :: Map String String
alias = Map.fromList
  [ ("\\bf", "\\textbf")
  , ("\\cal", "\\textcal")
  , ("\\it", "\\textit")
  , ("\\tt", "\\texttt")
  ]

names :: Set String
names = Set.fromList (Map.keys alias)

translate :: String -> String -> Either String String
translate command text =
  case Map.lookup command alias of
    Nothing   -> Left $ "Unknown command: " <> command
    Just cmd' -> Unary.translate cmd' text
