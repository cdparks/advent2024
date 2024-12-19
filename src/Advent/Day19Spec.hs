module Advent.Day19Spec
  ( spec
  ) where

import Advent.Prelude

import Advent.Input
import Advent.Parse
import Data.HashMap.Strict qualified as HashMap
import Data.List (isPrefixOf)
import Data.Trie (Trie)
import Data.Trie qualified as Trie

spec :: Spec
spec = parsing designs 19 $ do
  it "1" $ \Input{..} -> do
    uncurry part1 example `shouldBe` 6
    uncurry part1 problem `shouldBe` 238

  it "2" $ \Input{..} -> do
    uncurry part2 example `shouldBe` 16
    uncurry part2 problem `shouldBe` 635018909726691

part1 :: Trie -> [String] -> Int
part1 dict xs =
  length $ guard . search =<< xs
 where
  search = \case
    [] -> True
    c:cs -> or $ do
      suffix <- Trie.possibleSuffixes [c] dict
      guard $ suffix `isPrefixOf` cs
      pure $ search $ drop (length suffix) cs

part2 :: Trie -> [String] -> Int
part2 dict =
  sum . (`evalState` mempty) . traverse search
 where
  search = memo $ \case
    [] -> pure 1
    c:cs -> fmap sum . traverse search $ do
      candidate <- Trie.possibleSuffixes [c] dict
      guard $ candidate `isPrefixOf` cs
      pure $ drop (length candidate) cs

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

designs :: Parser (Trie, [String])
designs = (,)
  <$> fmap Trie.fromList (word `sepBy` ", ")
  <*  twoNewlines
  <*> many (word <* endOfLine)
 where
  word = many letter
