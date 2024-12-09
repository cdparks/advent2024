module Advent.Day09Spec
  ( spec
  ) where

import Advent.Prelude hiding (ix)

import Advent.Input
import Control.Monad.ST.Strict (runST)
import Data.Map.Strict qualified as Map
import Data.Sequence qualified as Seq
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as MU

spec :: Spec
spec = reading toSegments 9 $ do
  it "1" $ \Input{..} -> do
    part1 example `shouldBe` 1928
    part1 problem `shouldBe` 6390180901651

  it "2" $ \Input{..} -> do
    part2 example `shouldBe` 2858
    part2 problem `shouldBe` 6412390114238

part1 :: [Segment] -> Int
part1 =
  checksum . go . Seq.fromList
 where
  go = \case
    File x n :<| q -> replicate n x <> go q
    Free n :<| (q :|> File x m)
      | n > m -> replicate m x <> go (Free (n - m) :<| q)
      | n < m -> replicate n x <> go (q :|> File x (m - n))
      | otherwise -> replicate n x <> go q
    q :|> Free {} -> go q
    _ -> []

part2 :: [Segment] -> Int
part2 segments = checksum $ U.toList $ runST $ do
  v <- MU.new $ sum [n | (_, n, _) <- ixsegments]
  for_ ixsegments $ \(i, n, seg) ->
    splat v i n $ fromMaybe 0 seg
  go v free $ reverse ixsegments
  U.freeze v
 where
  go v fs = \case
    (i, n, Just x):xs -> do
      let (candidates, _, _) = Map.splitLookup i fs
      case filter ((n <=) . snd) $ Map.toList candidates of
        [] -> go v fs xs
        (j, m):_ -> do
          splat v j n x
          splat v i n 0
          let fs2 | n == m = Map.delete j fs
                  | otherwise = Map.insert (j + n) (m - n) $ Map.delete j fs
          go v fs2 xs
    _:xs -> go v fs xs
    [] -> pure ()

  ixsegments = indexed segments

  free = Map.fromList $ do
    (i, n, Nothing) <- ixsegments
    pure (i, n)

  splat v i n x =
    for_ [i .. i + n - 1] $ \j ->
      MU.unsafeWrite v j x

checksum :: [Int] -> Int
checksum = sum . zipWith (*) [0..]

indexed :: [Segment] -> [(Int, Int, Maybe Int)]
indexed = go 0
 where
  go !i = \case
    File x n : xs -> (i, n, Just x) : go (i + n) xs
    Free n : xs -> (i, n, Nothing) : go (i + n) xs
    [] -> []

data Segment = File Int Int | Free Int

toSegments :: Text -> [Segment]
toSegments =
  zipWith3 id (cycle [File, const Free]) ids
  . fmap (read . pure)
  . filter isDigit
  . unpack
 where
  ids = (:[-1]) =<< [0..]
