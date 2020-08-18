{-# LANGUAGE TupleSections #-}

module Spars (
    mapP,
    constP,
    idP,
    nullP,
    thenP,
    orP,
    eitherP
) where

import Utils

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
andP p1 p2 str = crossClasses (p1 str) (p2 str)