{-# OPTIONS_GHC -fno-warn-x-partial #-} -- Use of 'head'

module Advent.Day17Spec
  ( spec
  ) where

import Advent.Prelude hiding (Config)

import Advent.Input
import Advent.Parse
import Data.List (head)
import Data.Vector.Unboxed ((!), (!?))
import Data.Vector.Unboxed qualified as U

spec :: Spec
spec = parsing config 17 $ do
  it "1" $ \Input{..} -> do
    part1 example `shouldBe` [4,6,3,5,6,3,5,2,1,0]
    part1 problem `shouldBe` [4,6,1,4,2,1,3,1,6]

  it "2" $ \Input{..} -> do
    part2 problem `shouldBe` 202366627359274

part1 :: Config -> [Int]
part1 = run

part2 :: Config -> Int
part2 cfg@Config{..} =
  head $ go 0 $ reverse $ U.toList opcodes
 where
  go acc = \case
    [] -> pure acc
    x:xs -> do
      a <- ((acc * 8) +) <$> [0..7]
      let out = run cfg { bank = Bank a 0 0 }
      guard $ head out == x
      go a xs

run :: Config -> [Int]
run Config{..} =
  go 0 bank
 where
  go p regs@Bank{..} = case opcodes !? p of
    Nothing -> []
    Just opcode -> do
      let i = imm p
      let v = load regs p
      let next = go $ p + 2
      case opcode of
        Adv -> next regs { a = shiftR a v }
        Bdv -> next regs { b = shiftR a v }
        Cdv -> next regs { c = shiftR a v }
        Bxl -> next regs { b = xor b i }
        Bxc -> next regs { b = xor b c }
        Bst -> next regs { b = mod v 8 }
        Out -> mod v 8 : next regs
        Jnz | a == 0 -> next regs
            | otherwise -> go i regs
        other -> error $ "Unrecognized opcode: " <> show other

  imm p = opcodes ! (p + 1)
  load regs p = case imm p of
    RegA -> regs.a
    RegB -> regs.b
    RegC -> regs.c
    lit -> lit

data Bank = Bank
  { a :: Int
  , b :: Int
  , c :: Int
  }

data Config = Config
  { bank :: Bank
  , opcodes :: U.Vector Int
  }

pattern Adv, Bxl, Bst, Jnz, Bxc, Out, Bdv, Cdv :: Int
pattern Adv = 0
pattern Bxl = 1
pattern Bst = 2
pattern Jnz = 3
pattern Bxc = 4
pattern Out = 5
pattern Bdv = 6
pattern Cdv = 7

pattern RegA, RegB, RegC :: Int
pattern RegA = 4
pattern RegB = 5
pattern RegC = 6

config :: Parser Config
config = Config <$> bank <* twoNewlines <*> opcodes
 where
  bank = Bank <$> int <*> int <*> int
  opcodes = fmap U.fromList $ (:) <$> int <*> many ("," *> decimal)
  int = takeTill isDigit *> decimal
