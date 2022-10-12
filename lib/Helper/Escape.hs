module Helper.Escape where

import qualified Data.Map        as Map
import           Data.Set        (Set)
import qualified Data.Set        as Set

import qualified Helper.Mappings as Mappings


names :: Set String
names = Set.fromList (Map.keys Mappings.escapes)

translate :: String -> Either String String
translate name =
  case Map.lookup name Mappings.escapes of
    Nothing -> Left $ "Unknown command: " <> name
    Just r  -> Right r
