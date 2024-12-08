module Advent.Day08Spec
  ( spec
  ) where

import Advent.Prelude

import Advent.Input
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet

spec :: Spec
spec = reading toGrid 8 $ do
  it "1" $ \Input{..} -> do
    part1 example `shouldBe` 14
    part1 problem `shouldBe` 323

  it "2" $ \Input{..} -> do
    part2 example `shouldBe` 34
    part2 problem `shouldBe` 1077

part1 :: Grid -> Int
part1 g@Grid{..} = solve g $ \p q -> do
  let d = q `sub` p
  filter inBounds [p `sub` d]

part2 :: Grid -> Int
part2 g@Grid{..} = solve g $ \p q -> do
  let d = q `sub` p
  takeWhile inBounds $ iterate (`sub` d) p

solve
  :: Grid
  -> (Point -> Point -> [Point])
  -> Int
solve Grid{..} f = unique $ do
  (c, ps) <- HashMap.toList grid
  guard $ c /= '.'
  (p, q) <- (,) <$> ps <*> ps
  guard (q /= p)
  f p q
 where
  unique = HashSet.size . HashSet.fromList

sub :: Point -> Point -> Point
sub (!x1, !y1) (!x2, !y2) = (x1 - x2, y1 - y2)

type Point = (Int, Int)

data Grid = Grid
  { grid :: HashMap Char [Point]
  , inBounds :: Point -> Bool
  }

toGrid :: Text -> Grid
toGrid t = Grid {..}
 where
  grid = HashMap.fromListWith (<>) $ do
    (y, l) <- zip [0..] $ lines t
    (x, c) <- zip [0..] $ unpack l
    pure (c, [(x, y)])
  (xs, ys) = unzip $ concat $ HashMap.elems grid
  width = 1 + maximum xs
  height = 1 + maximum ys
  inBounds (x, y) = 0 <= x && x < width && 0 <= y && y < height
