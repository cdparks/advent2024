module Advent.Day03Spec
  ( spec
  ) where

import Advent.Prelude

import Advent.Input
import Advent.Parse

spec :: Spec
spec = parsing items 3 $ do
  it "1" $ \Input{..} -> do
    part1 example `shouldBe` 161
    part1 problem `shouldBe` 187833789

  it "2" $ \Input{..} -> do
    part2 example `shouldBe` 48
    part2 problem `shouldBe` 94455185

part1 :: [Item] -> Int
part1 xs = sum [i | Parsed i <- xs]

part2 :: [Item] -> Int
part2 =
  fst . foldl' step (0, 1)
 where
  step (!acc, !s) = \case
    Enabled -> (acc, 1)
    Disabled -> (acc, 0)
    Parsed i -> (i * s + acc, s)

data Item
  = Enabled
  | Disabled
  | Parsed Int

items :: Parser [Item]
items = many $ asum
  [ Disabled <$ string "don't()"
  , Enabled <$ string "do()"
  , Parsed <$> mul
  , Parsed 0 <$ anyChar
  ]

mul :: Parser Int
mul = (*)
  <$  string "mul("
  <*> decimal
  <*  char ','
  <*> decimal
  <*  char ')'
