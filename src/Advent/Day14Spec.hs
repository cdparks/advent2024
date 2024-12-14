module Advent.Day14Spec
  ( spec
  ) where

import Advent.Prelude

import Advent.Input
import Advent.Parse hiding (takeWhile)
import Data.HashMap.Strict qualified as HashMap

spec :: Spec
spec = parsing robots 14 $ do
  it "1" $ \Input{..} -> do
    part1 (11, 7) example `shouldBe` 12
    part1 (101, 103) problem `shouldBe` 233709840

  it "2" $ \Input{..} ->
    part2 (101, 103) problem `shouldBe` 6620

part1 :: (Int, Int) -> [Robot] -> Int
part1 (w, h) =
  product
  . HashMap.elems
  . HashMap.fromListWith (+)
  . map (,1)
  . mapMaybe (quadrant . teleport 100)
 where
  mx = w `div` 2
  my = h `div` 2

  teleport n r = r
    { x = (n * r.dx + r.x) `mod` w
    , y = (n * r.dy + r.y) `mod` h
    }

  quadrant r = do
    guard $ r.x /= mx
    guard $ r.y /= my
    pure (r.x < mx, r.y < my)

part2 :: (Int, Int) -> [Robot] -> Int
part2 (w, h) =
  length
  . takeWhile (not . has9Neighbors)
  . iterate step
  . HashMap.fromListWith (<>)
  . map (\(Robot {..}) -> ((x, y), [(dx, dy)]))
 where
  step m = HashMap.fromListWith (<>) $ do
    ((x, y), vs) <- HashMap.toList m
    (dx, dy) <- vs
    let q = ((dx + x) `mod` w, (dy + y) `mod` h)
    pure (q, [(dx, dy)])

has9Neighbors :: HashMap (Int, Int) [(Int, Int)] -> Bool
has9Neighbors m =
  any (uncurry isTriangle) $ HashMap.keys m
 where
  isTriangle x y = all (`HashMap.member` m) [(x + dx, y + dy) | (dx, dy) <- below]
  below =
    [ (-1, -1), (0,-1), (1, -1)
    , (-1,  0),         (1,  0)
    , (-1,  1), (0, 1), (1,  1)
    ]

data Robot = Robot
  { x :: Int
  , y :: Int
  , dx :: Int
  , dy :: Int
  }

robots :: Parser [Robot]
robots = (Robot <$> int <*> int <*> int <*> int) `sepBy` endOfLine
 where
  int = takeTill (\c -> c == '-' || isDigit c) *> signed decimal
