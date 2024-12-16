{-# OPTIONS_GHC -fno-warn-x-partial #-} -- Use of 'head' in 'part1'

module Advent.Day15Spec
  ( spec
  ) where

import Advent.Prelude

import Advent.Input
import Data.HashMap.Strict ((!))
import Data.HashMap.Strict qualified as HashMap
import Data.List (head)

spec :: Spec
spec = do
  reading toMap 15 $ do
    it "1" $ \Input{..} -> do
      uncurry part1 example `shouldBe` 10092
      uncurry part1 problem `shouldBe` 1463715

    it "2" $ \Input{..} -> do
      uncurry part2 (first expand example) `shouldBe` 9021
      uncurry part2 (first expand problem) `shouldBe` 1481392

part1 :: HashMap (Int, Int) Tile -> [Heading] -> Int
part1 = solve False

part2 :: HashMap (Int, Int) Tile -> [Heading] -> Int
part2 = solve True

solve :: Bool -> HashMap (Int, Int) Tile -> [Heading] -> Int
solve wide m0 =
  done . go m0 p0
 where
  p0 = head [p | (p, Robot) <- HashMap.toList m0]
  go m p = \case
    [] -> m
    h:hs
      | Just ps <- step m p h -> go (update m ps h) (move p h) hs
      | otherwise -> go m p hs

  step m p@(x, y) h = case HashMap.lookup p m of
    Nothing -> Just []
    Just Wall -> Nothing
    Just BoxL
      | wide && h `elem` [N, S] -> do
          let q = (x + 1, y)
          (p :) . (q :) <$> ((<>) <$> next p <*> next q)
      | otherwise -> (p :) <$> next p
    Just BoxR
      | wide && h `elem` [N, S] -> do
          let q = (x - 1, y)
          (p:) . (q:) <$> ((<>) <$> next p <*> next q)
      | otherwise -> (p :) <$> next p
    Just Robot -> (p :) <$> next p
   where
    next q = step m (move q h) h

  move (x, y) = \case
    N -> (x, y - 1)
    E -> (x + 1, y)
    W -> (x - 1, y)
    S -> (x, y + 1)

  done m = sum $ do
    ((x, y), BoxL) <- HashMap.toList m
    pure $ x + y * 100

  -- Much faster than union/difference
  update m ps h =
    foldr
      (\q -> HashMap.insert (move q h) (m ! q))
      (foldr HashMap.delete m ps)
      ps

data Tile = Wall | BoxL | BoxR | Robot

data Heading = N | E | W | S
  deriving Eq

toMap :: Text -> (HashMap (Int, Int) Tile, [Heading])
toMap t = first HashMap.fromList $ partitionEithers $ do
  (y, line) <- zip [0..] $ lines t
  (x, c) <- zip [0..] $ unpack line
  case c of
    '#' -> pure $ Left ((x, y), Wall)
    'O' -> pure $ Left ((x, y), BoxL)
    '@' -> pure $ Left ((x, y), Robot)
    '^' -> pure $ Right N
    '>' -> pure $ Right E
    '<' -> pure $ Right W
    'v' -> pure $ Right S
    _ -> []

expand :: HashMap (Int, Int) Tile -> HashMap (Int, Int) Tile
expand m = HashMap.fromList $ do
  ((x, y), t) <- HashMap.toList m
  case t of
    Wall -> (,Wall) <$> [(x * 2, y), (x * 2 + 1, y)]
    BoxL -> [((x * 2, y), BoxL), ((x * 2 + 1, y), BoxR)]
    BoxR -> []
    Robot -> [((x * 2, y), Robot)]
