{-# LANGUAGE TupleSections #-}

module Utils (crossClasses) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

crossClasses :: (Ord k, Ord a, Ord b) => Set.Set (a, k) -> Set.Set (b, k) -> Set.Set ((a, b), k)
crossClasses set1 set2 =
    let map1 = groupS' set1
        map2 = groupS' set2
        f key setA set0 =
            let setB = Map.findWithDefault Set.empty key map2
                setC = Set.cartesianProduct setA setB
                set_ = Set.map (, key) setC
            in  Set.union set0 set_
    in  Map.foldrWithKey f Set.empty map1

groupS' :: (Ord k, Ord a) => Set.Set (a, k) -> Map.Map k (Set.Set a)
groupS' = Map.unionsWith (Set.union) . Set.map (\(a, b) -> Map.singleton b (Set.singleton a))