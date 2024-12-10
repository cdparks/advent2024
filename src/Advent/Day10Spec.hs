module Advent.Day10Spec
  ( spec
  ) where

import Advent.Prelude

import Advent.Input
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.Sequence qualified as Seq

spec :: Spec
spec = reading toGraph 10 $ do
  it "1" $ \Input{..} -> do
    part1 example `shouldBe` 36
    part1 problem `shouldBe` 472

  it "2" $ \Input{..} -> do
    part2 example `shouldBe` 81
    part2 problem `shouldBe` 969

part1 :: Graph -> Int
part1 g = sum $ do
  ((x, y), 0) <- HashMap.toList g
  pure $ bfs g (x, y)

part2 :: Graph -> Int
part2 g = sum $ do
  ((x, y), 0) <- HashMap.toList g
  pure $ dfs g (x, y)

bfs :: Graph -> (Int, Int) -> Int
bfs g =
  go 0 mempty . Seq.singleton . (,0)
 where
  go !acc seen = \case
    Empty -> acc
    (p, i) :<| q
      | p `HashSet.member` seen -> go acc seen q
      | i == 9 -> go (acc + 1) updated q
      | otherwise -> go acc updated $ q <> more
     where
      updated = HashSet.insert p seen
      more = Seq.fromList (neighbors g p i)

dfs :: Graph -> (Int, Int) -> Int
dfs g s =
  go mempty s 0
 where
  go seen p i
    | p `HashSet.member` seen = 0
    | i == 9 = 1
    | otherwise = sum $ uncurry (go updated) <$> more
   where
    updated = HashSet.insert p seen
    more = neighbors g p i

neighbors :: Graph -> (Int, Int) -> Int -> [((Int, Int), Int)]
neighbors g (x, y) i = do
  q <- [(x, y - 1), (x - 1, y), (x, y + 1), (x + 1, y)]
  j <- maybeToList $ HashMap.lookup q g
  (q, j) <$ guard (i + 1 == j)

type Graph = HashMap (Int, Int) Int

toGraph :: Text -> Graph
toGraph t = HashMap.fromList $ do
  (y, line) <- zip [0..] $ lines t
  (x, c) <- zip [0..] $ unpack line
  pure ((x, y), ord c - ord '0')
