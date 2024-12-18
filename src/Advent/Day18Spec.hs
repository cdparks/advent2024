module Advent.Day18Spec
  ( spec
  ) where

import Advent.Prelude hiding (Empty)

import Advent.Input
import Advent.Parse
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.List ((!?))
import Data.PQueue.Min (pattern (:<), pattern Empty)
import Data.PQueue.Min qualified as Q
import Data.Semigroup (Arg(..))
import Numeric.Search.Range (searchFromTo)

spec :: Spec
spec = parsing points 18 $ do
  it "1" $ \Input{..} -> do
    part1 6 12 example `shouldBe` Just 22
    part1 70 1024 problem `shouldBe` Just 248

  it "2" $ \Input{..} -> do
    part2 6 example `shouldBe` Just (6, 1)
    part2 70 problem `shouldBe` Just (32, 55)

part1 :: Int -> Int -> [(Int, Int)] -> Maybe Int
part1 dst n =
  dijkstra (0, 0) (dst, dst)
  . HashSet.fromList
  . filter (`inBounds` (dst, dst))
  . take n

part2 :: Int -> [(Int, Int)] -> Maybe (Int, Int)
part2 dst ps =
  (ps !?) . subtract 1 =<< searchFromTo blocked 0 (length ps)
 where
  blocked i = isNothing $ part1 dst i ps

inBounds :: (Int, Int) -> (Int, Int) -> Bool
(x, y) `inBounds` (mx, my) = 0 <= x && x <= mx && 0 <= y && y <= my

dijkstra :: (Int, Int) -> (Int, Int) -> HashSet (Int, Int) -> Maybe Int
dijkstra start goal ps =
  loop (HashMap.singleton start 0)
    $ Q.singleton
    $ Arg @Int 0 start
 where
  loop seen0 = \case
    Empty -> Nothing
    Arg cost p@(x, y) :< q
      | p == goal -> Just cost
      | otherwise -> do
        let
          total = cost + 1
          ns = do
            n <- [(x, y-1), (x-1, y), (x, y+1), (x+1, y)]
            guard $ n `inBounds` goal
            guard $ not $ n `HashSet.member` ps
            guard $ total < HashMap.findWithDefault maxBound n seen0
            pure n
          seen = foldr (uncurry HashMap.insert . (,total)) seen0 ns
          more = Arg total <$> ns
        loop seen (q <> Q.fromList more)

points :: Parser [(Int, Int)]
points = point `sepBy` endOfLine
 where
  point = (,) <$> decimal <* "," <*> decimal
