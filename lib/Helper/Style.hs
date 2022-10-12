module Helper.Style where

import qualified Data.Map        as Map
import           Data.Set        (Set)
import qualified Data.Set        as Set

import qualified Helper.Mappings as Mappings
import qualified Helper.Unary    as Unary


names :: Set String
names = Set.fromList (Map.keys Mappings.alias)

translate :: String -> String -> Either String String
translate command text =
  case Map.lookup command Mappings.alias of
    Nothing   -> Left $ "Unknown command: " <> command
    Just cmd' -> Unary.translate cmd' text
