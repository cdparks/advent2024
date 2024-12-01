#!/usr/bin/env python3

import sys
import os.path
from pathlib import Path
import argparse

def main(days, error, force=False):
    modules = [
        (day, 'src/Advent/Day{:02}Spec.hs'.format(day))
        for day in days
    ]

    extant = ', '.join(
        module for (_, module) in modules
        if os.path.isfile(module)
    )

    if extant:
        if force:
            print('force overwriting {}'.format(extant))
        else:
            error('cowardly refusing to overwrite {}'.format(extant))

    for day, module in modules:
        with open(module, 'w') as f:
            f.write(template.format(day=day))

        touch('inputs/test{:02}.txt'.format(day))
        touch('inputs/day{:02}.txt'.format(day))

def touch(filename):
    '''Touch file at path'''
    return Path(filename).touch(exist_ok=True)

def parseDay(text):
    '''Parse a day as an integer within [1..25]'''
    day = int(text)
    if day < 1 or 25 < day:
        raise ValueError('day {} does not satisfy 1 <= day <= 25'.format(day))
    return day

# Haskell module template
template = '''module Advent.Day{day:02}Spec
  ( spec
  ) where

import Advent.Prelude

import Advent.Input
import Advent.Parse

spec :: Spec
spec = parsing takeText {day} $ do
  it "1" $ \\Input{{..}} -> do
    pendingWith "not implemented"
    part1 example `shouldBe` 1
    part1 problem `shouldBe` 1

  it "2" $ \\Input{{..}} -> do
    pendingWith "not implemented"
    part2 example `shouldBe` 2
    part2 problem `shouldBe` 2

part1 :: a -> Int
part1 = const $ negate 1

part2 :: a -> Int
part2 = const $ negate 2
'''

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='stub out Day{DAY}Spec.hs modules and empty input files')
    parser.add_argument('days', nargs='+', type=parseDay, metavar='DAY', help='day within the closed interval [1..25]')
    parser.add_argument('-f', '--force', action='store_true', help='overwrite extant modules')
    args = parser.parse_args()
    main(args.days, parser.error, args.force)
