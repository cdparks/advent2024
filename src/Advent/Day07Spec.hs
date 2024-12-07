{- HLINT ignore "Use ||" -}

module Advent.Day07Spec
  ( spec
  ) where

import Advent.Prelude

import Advent.Input
import Advent.Parse
import Data.List (isSuffixOf)

spec :: Spec
spec = parsing equations 7 $ do
  it "1" $ \Input{..} -> do
    part1 example `shouldBe` 3749
    part1 problem `shouldBe` 1620690235709

  it "2" $ \Input{..} -> do
    part2 example `shouldBe` 11387
    part2 problem `shouldBe` 145397611075341

part1 :: [Equation] -> Word
part1 = solve False

part2 :: [Equation] -> Word
part2 = solve True

solve :: Bool -> [Equation] -> Word
solve cat eqs = sum $ do
  (result, operands) <- eqs
  result <$ guard (sat result operands)
 where
  sat x = \case
    [] -> False
    [y] -> x == y
    y:ys -> do
      let (d, m) = x `divMod` y
      or [ m == 0 && sat d ys
         , cat && maybe False (`sat` ys) (x `stripSuffix` y)
         , x >= y && sat (x - y) ys
         ]

stripSuffix :: Word -> Word -> Maybe Word
stripSuffix x y = do
  guard $ ys `isSuffixOf` xs
  pure $ if null prefix then 0 else read prefix
 where
  xs = show x
  ys = show y
  prefix = take (length xs - length ys) xs

type Equation = (Word, [Word])

equations :: Parser [Equation]
equations =
  equation `sepBy` endOfLine
 where
  equation = (,)
    <$> decimal <* sym ": "
    <*> (reverse <$> decimal `sepBy` sym " ")
