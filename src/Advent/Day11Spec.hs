module Advent.Day11Spec
  ( spec
  ) where

import Advent.Prelude

import Advent.Input
import Data.IntMap.Strict qualified as IntMap
import Data.List ((!!))

spec :: Spec
spec = reading toInts 11 $ do
  it "1" $ \Input{..} -> do
    part1 example `shouldBe` 55312
    part1 problem `shouldBe` 203609

  it "2" $ \Input{..} ->
    part2 problem `shouldBe` 240954878211138

part1 :: [Int] -> Int
part1 = solve 25

part2 :: [Int] -> Int
part2 = solve 75

solve :: Int -> [Int] -> Int
solve n = sum
  . IntMap.elems
  . (!! n)
  . iterate step
  . IntMap.fromListWith (+)
  . map (,1)
 where
  step m = IntMap.fromListWith (+) $ do
    (x, c) <- IntMap.toList m
    y <- next x
    pure (y, c)

next :: Int -> [Int]
next x
  | x == 0 = [1]
  | m == 0 = [read hi, read lo]
  | otherwise = [x * 2024]
 where
  shown = show x
  size = length shown
  (d, m) = size `divMod` 2
  (hi, lo) = splitAt d shown

toInts :: Text -> [Int]
toInts = map (read . unpack) . splitOn " "
