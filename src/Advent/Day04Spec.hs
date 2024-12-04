module Advent.Day04Spec
  ( spec
  ) where

import Advent.Prelude

import Advent.Input
import Data.HashMap.Strict qualified as HashMap

spec :: Spec
spec = reading toGraph 4 $ do
  it "1" $ \Input{..} -> do
    part1 example `shouldBe` 18
    part1 problem `shouldBe` 2454

  it "2" $ \Input{..} -> do
    part2 example `shouldBe` 9
    part2 problem `shouldBe` 1858

part1 :: HashMap (Int, Int) Char -> Int
part1 g = sum $ do
  (p, 'X') <- HashMap.toList g
  d <- (,) <$> [-1, 0, 1] <*> [-1, 0, 1]
  guard $ "XMAS" == pick g (take 4 $ iterate (add d) p)
  pure 1

part2 :: HashMap (Int, Int) Char -> Int
part2 g = sum $ do
  (p, 'A') <- HashMap.toList g
  guard $ match p [(-1, -1), (0, 0), (1,  1)]
  guard $ match p [(-1,  1), (0, 0), (1, -1)]
  pure 1
 where
  match p qs = pick g (add p <$> qs) `elem` ["MAS", "SAM"]

pick :: Hashable k => HashMap k v -> [k] -> [v]
pick m = mapMaybe (`HashMap.lookup` m)

add :: (Int, Int) -> (Int, Int) -> (Int, Int)
add (!x1, !y1) (!x2, !y2) = (x1 + x2, y1 + y2)

toGraph :: Text -> HashMap (Int, Int) Char
toGraph t = HashMap.fromList $ do
  (y, line) <- zip [0..] $ lines t
  (x, c) <- zip [0..] $ unpack line
  pure ((x, y), c)
