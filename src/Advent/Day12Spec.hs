module Advent.Day12Spec
  ( spec
  ) where

import Advent.Prelude

import Advent.Input
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.Sequence qualified as Seq

spec :: Spec
spec = reading toPlots 12 $ do
  it "1" $ \Input{..} -> do
    part1 example `shouldBe` 1930
    part1 problem `shouldBe` 1483212

  it "2" $ \Input{..} -> do
    part2 example `shouldBe` 1206
    part2 problem `shouldBe` 897062

part1 :: Plots -> Int
part1 = solve $ \plots c ->
  sum . map ((4 -) . length . neighbors plots c)

part2 :: Plots -> Int
part2 = solve $ \plots c ->
  corners . sides plots c
 where
  corners fs = length $ do
    (f, p) <- HashSet.toList fs
    guard $ not $ (f, rot90 p f) `HashSet.member` fs

solve :: (Plots -> Char -> [Point] -> Int) -> Plots -> Int
solve price plots = sum $ do
  ((c, _), ps) <- HashMap.toList $ toRegions plots
  pure $ length ps * price plots c ps

toRegions :: Plots -> HashMap (Char, Int) [Point]
toRegions plots =
  run $ for_ (HashMap.toList plots) $ \(p, c) -> do
    visited <- gets $ HashMap.member p . (.seen)
    unless visited $ do
      let region = flood plots c p
      modify $ \Regions{..} -> Regions
        { regions = HashMap.insert (c, nextId) region regions
        , seen = seen `HashMap.union` HashMap.fromList [(q, nextId) | q <- region]
        , nextId = nextId + 1
        }
 where
  run = (.regions) . flip execState Regions
    { regions = mempty
    , seen = mempty
    , nextId = 0
    }

flood :: Plots -> Char -> Point -> [Point]
flood plots c =
  go mempty . Seq.singleton
 where
  go region = \case
    Empty -> HashSet.toList region
    p :<| ps
      | p `HashSet.member` region -> go region ps
      | otherwise -> do
          let updated = HashSet.insert p region
          let qs = ps <> Seq.fromList (neighbors plots c p)
          go updated qs

neighbors :: Plots -> Char -> Point -> [Point]
neighbors plots c (x, y) = do
  (_, q) <- neighborCoords (x, y)
  q <$ guard (HashMap.lookup q plots == Just c)

sides :: Plots -> Char -> [Point] -> HashSet (Face, Point)
sides plots c ps = HashSet.fromList $ do
  p <- ps
  (s, q) <- neighborCoords p
  (s, p) <$ guard (HashMap.lookup q plots /= Just c)

neighborCoords :: Point -> [(Face, Point)]
neighborCoords (x, y) =
  [ (N, (x+0, y-1))
  , (E, (x+1, y+0))
  , (W, (x-1, y+0))
  , (S, (x+0, y+1))
  ]

rot90 :: Point -> Face -> Point
rot90 (x, y) = \case
  N -> (x+1, y)
  E -> (x, y+1)
  W -> (x, y+1)
  S -> (x+1, y)

toPlots :: Text -> Plots
toPlots t = HashMap.fromList $ do
  (y, line) <- zip [0..] $ lines t
  (x, c) <- zip [0..] $ unpack line
  pure ((x, y), c)

type Point = (Int, Int)

data Face = N | E | W | S
  deriving stock (Eq, Generic)
  deriving anyclass Hashable

type Plots = HashMap Point Char

data Regions = Regions
  { regions :: HashMap (Char, Int) [Point]
  , seen :: HashMap Point Int
  , nextId :: Int
  }
