module Advent.Day13Spec
  ( spec
  ) where

import Advent.Prelude

import Advent.Input
import Advent.Parse

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

-- See https://en.wikipedia.org/wiki/Cramer%27s_rule#Explicit_formulas_for_small_systems
--
--   ⌈a₁ b₁⌉ ⌈x⌉ = ⌈c₁⌉
--   ⌊a₂ b₂⌋ ⌊y⌋ = ⌊c₂⌋
--
--       c₁b₂ - b₁c₂
--   x = ―――――――――――
--       a₁b₂ - b₁a₂
--
--       a₁c₂ - c₁a₂
--   y = ―――――――――――
--       a₁b₂ - b₁a₂
--
cost :: (Int -> Int) -> Machine -> Maybe Int
cost f ((a₁, a₂), (b₁, b₂), (f -> c₁, f -> c₂)) = do
  let det = a₁ * b₂ - b₁ * a₂
  (x, 0) <- pure $ (c₁ * b₂ - b₁ * c₂) `divMod` det
  (y, 0) <- pure $ (a₁ * c₂ - c₁ * a₂) `divMod` det
  pure $ x * 3 + y

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
