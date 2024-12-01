module Main
  ( main
  ) where

import Advent.Prelude

import Spec (spec)

main :: IO ()
main = hspec $ parallel spec
