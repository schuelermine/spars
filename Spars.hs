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

bindP :: Ord b => Parser a -> (a -> Parser b) -> Parser b
bindP p f str = Set.unions . Set.map (uncurry f) $ p str

combindP :: (Ord c, Ord b) => (a -> b -> c) -> Parser a -> (a -> Parser b) -> Parser c
combindP f p g str = Set.unions . Set.map q $ p str where
    q (x, s) = Set.map (first $ f x) $ g x s

join1P :: Ord a => Parser (Parser a) -> Parser a
join1P p str = Set.unions . Set.map (uncurry ($)) $ p str

join2P :: Ord a => Parser (Parser a) -> Parser a
join2P p str = Set.unions . Set.map (($ str) . fst) $ p str

discardP :: Parser a -> Parser ()
discardP p = Set.map (first $ const ()) . p

orP :: Ord a => Parser a -> Parser a -> Parser a
orP p1 p2 str = Set.union (p1 str) (p2 str)

eitherP :: (Ord a, Ord b) => Parser a -> Parser b -> Parser (Either a b)
eitherP p1 p2 str = 
    let left = (Set.map (first Left) $ p1 str)
        right = (Set.map (first Right) $ p2 str)
    in  Set.union left right

andP :: (Ord a, Ord b) => Parser a -> Parser b -> Parser (a, b)
andP p1 p2 str = crossClasses (p1 str) (p2 str)