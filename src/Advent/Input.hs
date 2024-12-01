{-# LANGUAGE StrictData #-}

module Advent.Input
  ( Input(..)
  , parsing
  , reading
  ) where

import Advent.Prelude

import Advent.Parse (Parser, parseIO)
import Text.Printf (printf)

-- | Parse inputs before all tests in a spec group run
parsing :: Parser a -> Int -> SpecWith (Input a) -> Spec
parsing parser = beforeAll . parse parser

-- | Read inputs before all tests in a spec group run
reading :: Int -> SpecWith (Input Text) -> Spec
reading = beforeAll . load

-- | Some test and problem input data
data Input a = Input
  { example :: a
  , problem :: a
  }
  deriving (Functor, Foldable, Traversable)

-- | Read and parse inputs
parse :: Parser a -> Int -> IO (Input a)
parse parser = traverse (parseIO parser) <=< load

-- | Read inputs
load :: Int -> IO (Input Text)
load day = Input
  <$> readFile ("inputs" </> "test" <> formatted)
  <*> readFile ("inputs" </> "day" <> formatted)
 where
  formatted = printf "%02d" day <> ".txt"
