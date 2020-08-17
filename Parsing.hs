{-# LANGUAGE TupleSections #-}

module Parsing (
    mapP,
    constP,
    idP,
    nullP,
    thenP,
    orP,
    eitherP
) where

import Data.These

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Bifunctor

type Parser t = String -> Set.Set (t, String)

mapP :: Ord b => (a -> b) -> Parser a -> Parser b
mapP f p s = Set.map (first f) (p s)

constP :: a -> Parser a
constP x _ = Set.singleton (x, "")

idP :: a -> Parser a
idP x s = Set.singleton (x, s)

nullP :: Parser a
nullP s = Set.empty

thenP :: (Ord a, Ord b) => Parser a -> Parser b -> Parser (a, b)
thenP p1 p2 str =
    let f (x, str) = Set.map (first (x,)) (p2 str)
        s1 = p1 str
        s2 = Set.unions $ Set.map f s1
    in  s2

orP :: Ord a => Parser a -> Parser a -> Parser a
orP p1 p2 str = Set.union (p1 str) (p2 str)

eitherP :: (Ord a, Ord b) => Parser a -> Parser b -> Parser (Either a b)
eitherP p1 p2 str = Set.union (Set.map (first Left) $ p1 str) (Set.map (first Right) $ p2 str)

theseP :: Parser a -> Parser b -> Parser (These a b)
theseP p1 p2 str = undefined

andP :: (Ord a, Ord b) => Parser a -> Parser b -> Parser (a, b)
andP p1 p2 str0 =
    let map1 = groupS $ p1 str0
        map1' = Map.toList map1
        map2 = groupS $ p2 str0
        f (str1, setA) set0 =
            let setB = Map.findWithDefault Set.empty str1 map2
                setC = Set.cartesianProduct setA setB
                set_ = Set.map (, str1) setC
            in  Set.union set0 set_
    in  foldr f Set.empty map1'

-- Auxiliary functions

groupS :: (Ord k, Ord a) => Set.Set (a, k) -> Map.Map k (Set.Set a)
groupS = Map.unionsWith (Set.union) . Set.map (uncurry $ (flip Map.singleton) . Set.singleton)