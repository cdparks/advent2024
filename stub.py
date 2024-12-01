#!/usr/bin/env python3

from pathlib import Path
from urllib.request import Request, urlopen
from urllib.error import HTTPError
import argparse
import os.path
import sys

def main(days, error, force=False):
    modules = [
        (day, f'src/Advent/Day{day:02}Spec.hs')
        for day in days
    ]

    extant = ', '.join(
        module for (_, module) in modules
        if os.path.isfile(module)
    )

    if extant:
        if force:
            print(f'force overwriting {extant}')
        else:
            error(f'cowardly refusing to overwrite {extant}')

    session = get_session(error)
    for day, module in modules:
        with open(module, 'w') as f:
            f.write(template.format(day=day))

        touch(f'inputs/test{day:02}.txt')

        input_file = f'inputs/day{day:02}.txt'
        if os.path.isfile(input_file) and not force:
            print(f'using cached input for day {day}')
        else:
            with open(f'inputs/day{day:02}.txt', 'w') as f:
                f.write(fetch(session, day))

def touch(filename):
    '''Touch file at path'''
    return Path(filename).touch(exist_ok=True)

def parseDay(text):
    '''Parse a day as an integer within [1..25]'''
    day = int(text)
    if day < 1 or 25 < day:
        raise ValueError('day {} does not satisfy 1 <= day <= 25'.format(day))
    return day

def get_session(error):
    '''Grab session cookie from AOC_SESSION env var or file'''
    session = os.getenv('AOC_SESSION')
    if session is not None:
        return session
    try:
        return open('AOC_SESSION').read()
    except Exception as e:
        error('set AOC_SESSION environment variable or write AOC_SESSION file')

def fetch(session, day):
    '''Fetch this day's puzzle input'''
    req = Request(f'https://adventofcode.com/2024/day/{day}/input')
    req.add_header('cookie', f'session={session}')
    req.add_header('user-agent', 'github.com/cdparks/advent2024')

    try:
        return urlopen(req).read().decode('utf-8')
    except HTTPError as e:
        if e.code == 404:
            print(f'input for day {day} not available yet; touching empty file')
            return ''
        raise e

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
