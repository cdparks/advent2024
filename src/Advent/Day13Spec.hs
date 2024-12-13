module Advent.Day13Spec
  ( spec
  ) where

import Advent.Prelude

import Advent.Input
import Advent.Parse
import Numeric.LinearAlgebra (Matrix, (><), linearSolve, toLists)

spec :: Spec
spec = parsing machines 13 $ do
  it "1" $ \Input{..} -> do
    part1 example `shouldBe` 480
    part1 problem `shouldBe` 39996

  it "2" $ \Input{..} -> do
    part2 example `shouldBe` 875318608908
    part2 problem `shouldBe` 73267584326867

part1 :: [Machine] -> Int
part1 = sum . mapMaybe (cost id)

part2 :: [Machine] -> Int
part2 = sum . mapMaybe (cost (+10000000000000))

cost :: (Int -> Int) -> Machine -> Maybe Int
cost f ((ax, ay), (bx, by), (f -> px, f -> py)) = do
  [n, m] <- map round . concat . toLists <$> linearSolve a b
  guard $ n * ax + m * bx == px
  guard $ n * ay + m * by == py
  pure $ n * 3 + m
 where
  a, b :: Matrix Double
  a = (2><2) $ fromIntegral <$> [ax, bx, ay, by]
  b = (2><1) $ fromIntegral <$> [px, py]

type Machine = ((Int, Int), (Int, Int), (Int, Int))

machines :: Parser [Machine]
machines = machine `sepBy` endOfLine

machine :: Parser Machine
machine = (,,) <$> point <*> point <*> point

point :: Parser (Int, Int)
point =
  (,) <$> int <*> int <* endOfLine
 where
  int = takeTill isDigit *> decimal
