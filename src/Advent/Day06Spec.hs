{-# LANGUAGE StrictData #-}

module Advent.Day06Spec
  ( spec
  ) where

import Advent.Prelude hiding (empty)

import Advent.Input
import Data.HashSet qualified as HashSet

spec :: Spec
spec = reading toMap 6 $ do
  it "1" $ \Input{..} -> do
    part1 example `shouldBe` 41
    part1 problem `shouldBe` 5329

  it "2" $ \Input{..} -> do
    part2 example `shouldBe` 6
    part2 problem `shouldBe` 2162

part1 :: Grid -> Int
part1 = HashSet.size . search

part2 :: Grid -> Int
part2 g =
  sum $ parFor ps $ \p ->
    if hasCycle g { block = HashSet.insert p g.block }
      then 1
      else 0
 where
  ps = HashSet.toList $ search g
  parFor = flip $ parMap rpar

search :: Grid -> HashSet (Int, Int)
search Grid{ start = Just s, ..} =
  go (HashSet.singleton s) (0, -1) s
 where
  go !seen v p
    | outOfBounds               = seen
    | p' `HashSet.member` block = go seen v' p
    | otherwise                 = go (HashSet.insert p' seen) v p'
   where
    p'@(x, y) = p `add` v
    v' = rot90 v
    outOfBounds = x < 0 || x > width || y < 0 || y > height
search _ = HashSet.empty

hasCycle :: Grid -> Bool
hasCycle Grid{ start = Just s, ..} = do
  let up = (0, -1)
  go (HashSet.singleton (up, s)) up s
 where
  go !seen v p
    | outOfBounds               = False
    | p' `HashSet.member` block = (v', p ) `HashSet.member` seen || go (HashSet.insert (v', p ) seen) v' p
    | otherwise                 = (v , p') `HashSet.member` seen || go (HashSet.insert (v , p') seen) v  p'
   where
    p'@(x, y) = p `add` v
    v' = rot90 v
    outOfBounds = x < 0 || x > width || y < 0 || y > height
hasCycle _ = False

add :: (Int, Int) -> (Int, Int) -> (Int, Int)
add (!x1, !y1) (!x2, !y2) = (x1 + x2, y1 + y2)

rot90 :: (Int, Int) -> (Int, Int)
rot90 = \case
  ( 0, -1) -> ( 1,  0)
  ( 1,  0) -> ( 0,  1)
  ( 0,  1) -> (-1,  0)
  (-1,  0) -> ( 0, -1)
  _ -> error "not a uniq vector"

data Grid = Grid
  { start :: Maybe (Int, Int)
  , block :: HashSet (Int, Int)
  , width :: Int
  , height :: Int
  }
  deriving Show

instance Semigroup Grid where
  Grid s1 b1 w1 h1 <> Grid s2 b2 w2 h2 = Grid (firstJust s1 s2) (b1 <> b2) (w1 `max` w2) (h1 `max` h2)
   where
    firstJust (Just x) _ = Just x
    firstJust _ (Just y) = Just y
    firstJust _ _ = Nothing

instance Monoid Grid where
  mempty = Grid Nothing mempty 0 0

toMap :: Text -> Grid
toMap t = mconcat $ do
  (y, line) <- zip [0..] $ lines t
  (x, c) <- zip [0..] $ unpack line
  let g0 = mempty { width = x, height = y }
  pure $ case c of
    '^' -> g0 { start = Just (x, y) }
    '#' -> g0 { block = HashSet.singleton (x, y) }
    _   -> g0
