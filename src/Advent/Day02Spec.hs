module Advent.Day02Spec
  ( spec
  ) where

import Advent.Prelude

import Advent.Input
import Advent.Parse

spec :: Spec
spec = parsing levels 2 $ do
  it "1" $ \Input{..} -> do
    part1 example `shouldBe` 2
    part1 problem `shouldBe` 369

  it "2" $ \Input{..} -> do
    part2 example `shouldBe` 4
    part2 problem `shouldBe` 428

part1 :: [[Int]] -> Int
part1 = countSafe pure

part2 :: [[Int]] -> Int
part2 =
  countSafe $ \xs -> xs : delete1 xs
 where
  delete1 = \case
    [] -> []
    x:xs -> xs : map (x:) (delete1 xs)

countSafe :: ([Int] -> [[Int]]) -> [[Int]] -> Int
countSafe f =
  length . filter safe
 where
  safe :: [Int] -> Bool
  safe xs = or $ do
    ys <- f xs
    let diffs = zipWith (-) ys $ drop 1 ys
    pure $ all inc diffs || all dec diffs

  inc :: Int -> Bool
  inc x = 1 <= x && x <= 3

  dec :: Int -> Bool
  dec x = -3 <= x && x <= -1

levels :: Parser [[Int]]
levels =
  report `sepBy` char '\n'
 where
  report = decimal `sepBy1` char ' '
