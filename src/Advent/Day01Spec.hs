module Advent.Day01Spec
  ( spec
  ) where

import Advent.Prelude

import Advent.Input
import Advent.Parse
import Data.Map.Strict qualified as Map

spec :: Spec
spec = parsing pair 1 $ do
  it "1" $ \Input{..} -> do
    uncurry part1 example `shouldBe` 11
    uncurry part1 problem `shouldBe` 2113135

  it "2" $ \Input{..} -> do
    uncurry part2 example `shouldBe` 31
    uncurry part2 problem `shouldBe` 19097157

part1 :: [Int] -> [Int] -> Int
part1 xs ys =
  sum $ zipWith diff (sort xs) (sort ys)
 where
  diff x y = abs (x - y)

part2 :: [Int] -> [Int] -> Int
part2 xs ys =
  sum [x * Map.findWithDefault 0 x freq | x <- xs ]
 where
  freq = Map.fromListWith (+) $ (,1) <$> ys

pair :: Parser ([Int], [Int])
pair = unzip <$> many ((,) <$> token decimal <*> token decimal)
