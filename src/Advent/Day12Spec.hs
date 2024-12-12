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

data Groups = Groups
  { groups :: HashMap (Char, Int) [Point]
  , seen :: HashMap Point Int
  , nextId :: Int
  }

type Point = (Int, Int)
data Face = N | E | W | S
  deriving Eq

part1 :: HashMap Point Char -> Int
part1 plots = done $ run $ do
  for_ (HashMap.toList plots) $ \(p, c) -> do
    seen <- gets (.seen)
    unless (p `HashMap.member` seen) $ do
      let group = flood c p
      modify $ \gs -> gs
        { groups = HashMap.insert (c, gs.nextId) group gs.groups
        , seen = gs.seen `HashMap.union` HashMap.fromList [(q, gs.nextId) | q <- group]
        , nextId = gs.nextId + 1
        }
 where
  run :: State Groups () -> HashMap (Char, Int) [Point]
  run = (.groups) . flip execState Groups
    { groups = mempty
    , seen = mempty
    , nextId = 0
    }

  flood :: Char -> Point -> [Point]
  flood c = go mempty . Seq.singleton
   where
    go group = \case
      p :<| ps
        | p `HashSet.member` group -> go group ps
        | otherwise -> go (HashSet.insert p group) (ps <> Seq.fromList (neighbors c p))
      Empty -> HashSet.toList group

  done :: HashMap (Char, Int) [Point] -> Int
  done groups = sum $ do
    ((c, _), ps) <- HashMap.toList groups
    pure $ length ps * sum (uncovered c <$> ps)

  neighbors :: Char -> Point -> [Point]
  neighbors c (x, y) = do
    q <- neighborCoords (x, y)
    q <$ guard (HashMap.lookup q plots == Just c)

  neighborCoords :: Point -> [Point]
  neighborCoords (x, y) = [(x+0, y-1), (x-1, y+0), (x+0, y+1), (x+1, y+0)]

  uncovered :: Char -> Point -> Int
  uncovered c = (4 -) . length . neighbors c

part2 :: HashMap Point Char -> Int
part2 plots = done $ run $ do
  for_ (HashMap.toList plots) $ \(p, c) -> do
    seen <- gets (.seen)
    unless (p `HashMap.member` seen) $ do
      let group = flood c p
      modify $ \gs -> gs
        { groups = HashMap.insert (c, gs.nextId) group gs.groups
        , seen = gs.seen `HashMap.union` HashMap.fromList [(q, gs.nextId) | q <- group]
        , nextId = gs.nextId + 1
        }
 where
  run :: State Groups () -> HashMap (Char, Int) [Point]
  run = (.groups) . flip execState Groups
    { groups = mempty
    , seen = mempty
    , nextId = 0
    }

  flood :: Char -> Point -> [Point]
  flood c = go mempty . Seq.singleton
   where
    go group = \case
      p :<| ps
        | p `HashSet.member` group -> go group ps
        | otherwise -> go (HashSet.insert p group) (ps <> Seq.fromList (neighbors c p))
      Empty -> HashSet.toList group

  done :: HashMap (Char, Int) [Point] -> Int
  done groups = sum $ do
    ((c, _), ps) <- HashMap.toList groups
    pure $ length ps * straights (sides c =<< ps)

  neighbors :: Char -> Point -> [Point]
  neighbors c (x, y) = do
    (_, q) <- neighborCoords (x, y)
    q <$ guard (HashMap.lookup q plots == Just c)

  sides :: Char -> Point -> [(Face, Point)]
  sides c p = do
    (s, q) <- neighborCoords p
    (s, p) <$ guard (HashMap.lookup q plots /= Just c)

  straights :: [(Face, Point)] -> Int
  straights fs = (length fs -) . length $ do
    (i :: Int, (f0, (x0, y0))) <- zip [0..] fs
    (j :: Int, (f1, (x1, y1))) <- zip [0..] fs
    guard $ j > i
    guard
      $  x0 == x1 && abs (y0 - y1) == 1 && f0 == f1
      || y0 == y1 && abs (x0 - x1) == 1 && f0 == f1

  neighborCoords :: Point -> [(Face, Point)]
  neighborCoords (x, y) =
    [ (N, (x+0, y-1))
    , (E, (x+1, y+0))
    , (W, (x-1, y+0))
    , (S, (x+0, y+1))
    ]

toPlots :: Text -> HashMap Point Char
toPlots t = HashMap.fromList $ do
  (y, line) <- zip [0..] $ lines t
  (x, c) <- zip [0..] $ unpack line
  pure ((x, y), c)
