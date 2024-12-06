{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -fno-warn-x-partial #-} -- Use of 'head' in 'toGrid'

module Advent.Day06Spec
  ( spec
  ) where

import Advent.Prelude

import Advent.Input
import Control.Monad.ST.Strict (runST, ST)
import Data.HashSet qualified as HashSet
import Data.List (head)
import Data.Vector.Unboxed ((!))
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as MU

spec :: Spec
spec = reading toGrid 6 $ do
  it "1" $ \Input{..} -> do
    part1 example `shouldBe` 41
    part1 problem `shouldBe` 5329

  it "2" $ \Input{..} -> do
    part2 example `shouldBe` 6
    part2 problem `shouldBe` 2162

part1 :: Grid -> Int
part1 = HashSet.size . search

part2 :: Grid -> Int
part2 g@Grid{..} = sum $ runST $ do
  tiles <- U.thaw grid
  seen <- MU.replicate (width * height) 0
  for (zip3 [1..] (start:qs) qs) $ \(gen, (px, py), (qx, qy)) -> do
    MU.write tiles (px + py * width) 0
    MU.write tiles (qx + qy * width) 1
    found <- hasCycle g gen tiles seen
    pure $ if found then 1 else 0
 where
  qs = HashSet.toList $ search g

search :: Grid -> HashSet (Int, Int)
search Grid {..} =
  go (HashSet.singleton start) (0, -1) start
 where
  go !seen v p
    | oob = seen
    | obstacle = go seen v' p
    | otherwise = go (HashSet.insert p' seen) v p'
   where
    p'@(x, y) = p `add` v
    v' = rot90 v
    oob = x < 0 || x >= width || y < 0 || y >= height
    obstacle = grid ! (x + y * width) == 1

hasCycle :: forall s. Grid -> Word -> MU.MVector s Word8 -> MU.MVector s Word -> ST s Bool
hasCycle Grid{ start = start@(sx, sy), ..} gen tiles seen = do
  let up = (0, -1)
  MU.write seen (sx + sy * width) $ (gen `shiftL` 4) .|. flag up
  go up start
 where
  go v p@(x, y)
    | oob = pure False
    | otherwise = MU.unsafeRead tiles i' >>= \case
      0 -> update i' v   p' -- free
      _ -> update i  v'  p  -- obstacle
   where
    p'@(x', y') = p `add` v
    v' = rot90 v
    oob = x' < 0 || x' >= width || y' < 0 || y' >= height
    i = x + y * width
    i' = x' + y' * width

  update i v p = MU.unsafeRead seen i >>= \case
    s | s `shiftR` 4 == gen -> do
      -- state is current generation, check if we've been here
      if (s .&. flag v) /= 0
         -- already visited tile w/ this direction
        then pure True
        else do
          -- add this direction, keep going
          MU.unsafeWrite seen i (s .|. flag v)
          go v p
    _ -> do
      -- visited is old generation, set new one w/ direction, keep going
      MU.unsafeWrite seen i ((gen `shiftL` 4) .|. flag v)
      go v p
  {-# INLINE update #-}

rot90 :: (Int, Int) -> (Int, Int)
rot90 = \case
  ( 0, -1) -> ( 1,  0)
  ( 1,  0) -> ( 0,  1)
  ( 0,  1) -> (-1,  0)
  (-1,  0) -> ( 0, -1)
  _ -> error "not a unit vector"
{-# INLINE rot90 #-}

flag :: (Int, Int) -> Word
flag = \case
  ( 0, -1) -> 1
  ( 1,  0) -> 2
  ( 0,  1) -> 4
  (-1,  0) -> 8
  _ -> error "not a unit vector"
{-# INLINE flag #-}

add :: (Int, Int) -> (Int, Int) -> (Int, Int)
add (!x1, !y1) (!x2, !y2) = (x1 + x2, y1 + y2)

data Grid = Grid
  { start :: (Int, Int)
  , grid :: U.Vector Word8
  , width :: Int
  , height :: Int
  }

toGrid :: Text -> Grid
toGrid t = Grid
  { start = head [p | (p, 2) <- ps]
  , grid = runST $ do
      vs <- MU.new (width * height)
      for_ ps $ \((x, y), i) -> do
        MU.unsafeWrite vs (x + y * width) (i .&. 1)
      U.freeze vs
  , width
  , height
  }
 where
  width = 1 + maximum [x | ((x, _), _) <- ps]
  height = 1 + maximum [y | ((_, y), _) <- ps]
  ps = do
    (y, line) <- zip [0..] $ lines t
    (x, c) <- zip [0..] $ unpack line
    pure $ case c of
      '^' -> ((x, y), 2)
      '#' -> ((x, y), 1)
      _   -> ((x, y), 0)
