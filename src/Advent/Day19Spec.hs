module Advent.Day19Spec
  ( spec
  ) where

import Advent.Prelude

import Advent.Input
import Advent.Parse
import Data.HashMap.Strict qualified as HashMap

spec :: Spec
spec = parsing designs 19 $ do
  it "1" $ \Input{..} -> do
    uncurry part1 example `shouldBe` 6
    uncurry part1 problem `shouldBe` 238

  it "2" $ \Input{..} -> do
    uncurry part2 example `shouldBe` 16
    uncurry part2 problem `shouldBe` 635018909726691

part1 :: [String] -> [String] -> Int
part1 dict xs =
  length $ guard . search =<< xs
 where
  search = \case
    [] -> True
    cs -> or $ do
      candidate <- dict
      search <$> maybeToList (dropPrefix candidate cs)

part2 :: [String] -> [String] -> Int
part2 dict =
  sum . (`evalState` mempty) . traverse search
 where
  search = memo $ \case
    [] -> pure 1
    cs -> fmap sum . traverse search $ do
      candidate <- dict
      maybeToList $ dropPrefix candidate cs

dropPrefix :: Eq a => [a] -> [a] -> Maybe [a]
dropPrefix [] ys = Just ys
dropPrefix (x:xs) (y:ys)
  | x == y = dropPrefix xs ys
dropPrefix _ _ = Nothing

memo
  :: (Hashable k, MonadState (HashMap k v) m)
  => (k -> m v)
  -> k
  -> m v
memo f k = gets (HashMap.lookup k) >>= \case
  Just v -> pure v
  Nothing -> do
    v <- f k
    v <$ modify (HashMap.insert k v)

designs :: Parser ([String], [String])
designs = (,)
  <$> word `sepBy` ", "
  <*  twoNewlines
  <*> many (word <* endOfLine)
 where
  word = many letter
