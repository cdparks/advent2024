module Advent.Day05Spec
  ( spec
  ) where

import Advent.Prelude

import Advent.Input
import Advent.Parse
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.List ((!!))

spec :: Spec
spec = parsing input 5 $ do
  it "1" $ \Input{..} -> do
    uncurry part1 example `shouldBe` 143
    uncurry part1 problem `shouldBe` 6267

  it "2" $ \Input{..} -> do
    uncurry part2 example `shouldBe` 123
    uncurry part2 problem `shouldBe` 5184

part1 :: Rules -> [[Int]] -> Int
part1 rules updates = sum $ do
  update <- updates
  guard $ isSorted rules update
  pure $ middle update

part2 :: Rules -> [[Int]] -> Int
part2 rules updates = sum $ do
  update <- updates
  guard $ not $ isSorted rules update
  pure $ middle $ sortBy (compareVia rules) update

isSorted :: Rules -> [Int] -> Bool
isSorted rules xs = and . zipWith (isBefore rules) xs $ drop 1 xs

isBefore :: Rules -> Int -> Int -> Bool
isBefore rules x y = y `IntSet.member` IntMap.findWithDefault mempty x rules

middle :: [a] -> a
middle xs = xs !! (length xs `div` 2)

compareVia :: Rules -> Int -> Int -> Ordering
compareVia rules x y
  | x == y = EQ
  | isBefore rules x y = LT
  | otherwise = GT

type Rules = IntMap IntSet

input :: Parser (Rules, [[Int]])
input = (,) . IntMap.fromListWith IntSet.union
  <$> many rule
  <*  endOfLine
  <*> many (decimal `sepBy` sym "," <* endOfLine)
 where
  rule = do
    x <- decimal <* sym "|"
    (x,) . IntSet.singleton <$> decimal <* endOfLine
