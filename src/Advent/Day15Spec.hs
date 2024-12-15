{-# OPTIONS_GHC -fno-warn-x-partial #-} -- Use of 'head' in 'part1'

module Advent.Day15Spec
  ( spec
  ) where

import Advent.Prelude

import Advent.Input
import Data.HashMap.Strict qualified as HashMap
import Data.List (head)

spec :: Spec
spec = do
  reading (toMapWith pure) 15 $ do
    it "1" $ \Input{..} -> do
      uncurry part1 example `shouldBe` 10092
      uncurry part1 problem `shouldBe` 1463715

  reading (toMapWith expand) 15 $ do
    it "2" $ \Input{..} -> do
      uncurry part2 example `shouldBe` 9021
      uncurry part2 problem `shouldBe` 1481392
 where
  expand (x, y) = [(x * 2, y), (x * 2 + 1, y)]

part1 :: HashMap (Int, Int) Tile -> [Heading] -> Int
part1 = solve False

part2 :: HashMap (Int, Int) Tile -> [Heading] -> Int
part2 = solve True

solve :: Bool -> HashMap (Int, Int) Tile -> [Heading] -> Int
solve wide m0 = gps . go m0 p0
 where
  p0 = head [p | (p, Robot) <- HashMap.toList m0]
  go m p = \case
    [] -> m
    h:hs
      | Just n <- step m [(q, Robot)] h -> go n q hs
      | otherwise -> go m p hs
     where
      q = move p h

  step m ps h
    | null ps = Just m
    | otherwise = case bimap concat concat . unzip <$> traverse (next m h) ps of
        Nothing -> Nothing
        Just (qs, dels) -> do
          -- This is way faster than using Union/Difference, probably for laziness reasons
          let del = [q | q <- dels, q `notElem` map fst ps]
          let updated = foldr (uncurry HashMap.insert) (foldr HashMap.delete m del) ps
          step updated qs h

  next m h (p@(x, y), _tile) =
    case HashMap.lookup p m of
      Just Wall ->
        Nothing
      Just BoxL
        | wide && h `elem` [N, S] ->
          Just ([(move p h, BoxL), (move (x + 1, y) h, BoxR)], [(x + 1, y)])
        | otherwise ->
          Just ([(move p h, BoxL)], [])
      Just BoxR
        | wide && h `elem` [N, S] ->
          Just ([(move p h, BoxR), (move (x - 1, y) h, BoxL)], [(x - 1, y)])
        | otherwise ->
          Just ([(move p h, BoxR)], [])
      Just Robot ->
        Just ([], [])
      Nothing ->
        Just ([], [])

  move (x, y) = \case
    N -> (x, y - 1)
    E -> (x + 1, y)
    W -> (x - 1, y)
    S -> (x, y + 1)

  gps m = sum $ do
    ((x, y), BoxL) <- HashMap.toList m
    pure $ x + y * 100

data Tile = Wall | BoxL | BoxR | Robot

data Heading = N | E | W | S
  deriving Eq

toMapWith :: ((Int, Int) -> [(Int, Int)]) -> Text -> (HashMap (Int, Int) Tile, [Heading])
toMapWith f t = first HashMap.fromList $ partitionEithers $ do
  (y, line) <- zip [0..] $ lines t
  (x, c) <- zip [0..] $ unpack line
  case c of
    '#' -> Left . (,Wall) <$> f (x, y)
    'O' -> Left <$> zip (f (x, y)) [BoxL, BoxR]
    '@' -> take 1 $ Left . (,Robot) <$> f (x, y)
    '^' -> pure $ Right N
    '>' -> pure $ Right E
    '<' -> pure $ Right W
    'v' -> pure $ Right S
    _ -> []
