module Advent.Day06Spec
  ( spec
  ) where

import Advent.Prelude hiding (empty)

import Advent.Input
import Data.HashSet qualified as HashSet
import Data.Monoid (Last(..))

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
part2 g = sum $ do
  p <- HashSet.toList $ search g
  1 <$ guard
    ( hasCycle g
      { empty = HashSet.delete p g.empty
      , block = HashSet.insert p g.block
      }
    )

search :: Grid -> HashSet (Int, Int)
search Grid{ start = Last (Just s), ..} =
  go (HashSet.singleton s) (0, -1) s
 where
  go !seen v p
    | p' `HashSet.member` empty = go (HashSet.insert p' seen) v p'
    | p' `HashSet.member` block = go seen v' p
    | otherwise = seen
   where
    p' = p `add` v
    v' = rot90 v
search _ = HashSet.empty

hasCycle :: Grid -> Bool
hasCycle Grid{ start = Last (Just s), ..} = do
  let up = (0, -1)
  go (HashSet.singleton (up, s)) up s
 where
  go !seen v p
    | isEmpty = (v , p') `HashSet.member` seen || go (HashSet.insert (v , p') seen) v  p'
    | isBlock = (v', p ) `HashSet.member` seen || go (HashSet.insert (v', p ) seen) v' p
    | otherwise = False
   where
    p' = p `add` v
    v' = rot90 v
    isEmpty = p' `HashSet.member` empty
    isBlock = p' `HashSet.member` block
hasCycle _ = False

add :: (Int, Int) -> (Int, Int) -> (Int, Int)
add (!x1, !y1) (!x2, !y2) = (x1 + x2, y1 + y2)

rot90 :: (Int, Int) -> (Int, Int)
rot90 = \case
  -- N -> E
  ( 0, -1) -> ( 1,  0)
  -- E -> S
  ( 1,  0) -> ( 0,  1)
  -- S -> W
  ( 0,  1) -> (-1,  0)
  -- W -> N
  (-1,  0) -> ( 0, -1)
  _ -> error "not a uniq vector"

data Grid = Grid
  { start :: Last (Int, Int)
  , block :: HashSet (Int, Int)
  , empty :: HashSet (Int, Int)
  }
  deriving Show

instance Semigroup Grid where
  Grid s1 b1 e1 <> Grid s2 b2 e2 = Grid (s1 <> s2) (b1 <> b2) (e1 <> e2)

instance Monoid Grid where
  mempty = Grid mempty mempty mempty

toMap :: Text -> Grid
toMap t = mconcat $ do
  (y, line) <- zip [0..] $ lines t
  (x, c) <- zip [0..] $ unpack line
  pure $ case c of
    '^' -> mempty
      { start = Last $ Just (x, y)
      , empty = HashSet.singleton (x, y)
      }
    '#' -> mempty { block = HashSet.singleton (x, y) }
    '.' -> mempty { empty = HashSet.singleton (x, y) }
    _   -> mempty
