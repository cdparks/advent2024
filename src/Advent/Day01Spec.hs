module Advent.Day01Spec
  ( spec
  ) where

import Advent.Prelude

import Advent.Input
import Advent.Parse

spec :: Spec
spec = parsing takeText 1 $ do
  it "1" $ \Input{..} -> do
    pendingWith "not implemented"
    part1 example `shouldBe` 1
    part1 problem `shouldBe` 1

  it "2" $ \Input{..} -> do
    pendingWith "not implemented"
    part2 example `shouldBe` 2
    part2 problem `shouldBe` 2

part1 :: a -> Int
part1 = const $ negate 1

part2 :: a -> Int
part2 = const $ negate 2
