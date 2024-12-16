{-# OPTIONS_GHC -fno-warn-x-partial #-} -- Use of 'head'

module Advent.Day16Spec
  ( spec
  ) where

import Advent.Prelude hiding (Empty, Arg)

import Advent.Input
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.List (head)
import Data.PQueue.Min (pattern (:<), pattern Empty)
import Data.PQueue.Min qualified as Q
import Data.Semigroup (Arg(..))

spec :: Spec
spec = reading toMap 16 $ do
  it "1" $ \Input{..} -> do
    part1 example `shouldBe` 7036
    part1 problem `shouldBe` 143564

  it "2" $ \Input{..} -> do
    part2 example `shouldBe` 45
    part2 problem `shouldBe` 593

part1 :: HashMap (Int, Int) Tile -> Int
part1 = maybe 0 (fst . fst) . uncons . dijkstra

part2 :: HashMap (Int, Int) Tile -> Int
part2 = HashSet.size . HashSet.unions . map snd. dijkstra

dijkstra :: HashMap (Int, Int) Tile -> [(Int, HashSet (Int, Int))]
dijkstra m =
  loop maxBound (HashMap.singleton (p0, E) 0)
    $ Q.singleton
    $ Arg @Int 0 (p0, E, HashSet.singleton p0)
 where
  p0 = head [p | (p, Start) <- HashMap.toList m]

  loop minCost seen0 = \case
    Empty -> []
    Arg cost (p, h, ts) :< q
      | Just Goal <- HashMap.lookup p m
      , cost <= minCost -> (cost, ts) : loop cost seen0 q
      | otherwise -> do
        let
          (costs, more) = unzip $ do
            (c, n, i) <- neighbors p h
            let total = cost + c
            guard $ total <= HashMap.findWithDefault maxBound (n, i) seen0
            pure (((n, i), total), Arg total (n, i, HashSet.insert n ts))
          seen = foldr (uncurry HashMap.insert) seen0 costs
        loop minCost seen (q <> Q.fromList more)

  neighbors p h = do
    let q = step p h
    map (1000,p,) [left h, right h]
      <> [ (1, q, h)
         | HashMap.lookup q m /= Just Wall
         ]

left :: Heading -> Heading
left = \case
  N -> W
  E -> N
  S -> E
  W -> S

right :: Heading -> Heading
right = \case
  N -> E
  E -> S
  S -> W
  W -> N

step :: (Int, Int) -> Heading -> (Int, Int)
step (x, y) = \case
  N -> (x, y - 1)
  E -> (x + 1, y)
  S -> (x, y + 1)
  W -> (x - 1, y)

data Tile = Start | Wall | Goal
  deriving Eq

data Heading = N | E | S | W
  deriving stock (Eq, Generic)
  deriving anyclass (Hashable)

toMap :: Text -> HashMap (Int, Int) Tile
toMap t = HashMap.fromList $ do
  (y, line) <- zip [0..] $ lines t
  (x, c) <- zip [0..] $ unpack line
  ((x,y),) <$> case c of
    '#' -> pure Wall
    'S' -> pure Start
    'E' -> pure Goal
    _ -> []
