module Overload.Internal where

import qualified Data.Map as Map
import qualified Data.Set as Set


adjustWithDefault :: Ord k => (a -> a) -> a -> k -> Map.Map k a -> Map.Map k a
adjustWithDefault f def = Map.alter (Just . maybe def f)

toMaybe :: Bool -> a -> Maybe a
toMaybe True  = Just
toMaybe False = const Nothing

disjointKeys :: Ord k => Map.Map k v -> Set.Set k -> Bool
disjointKeys m = Map.null . Map.restrictKeys m
