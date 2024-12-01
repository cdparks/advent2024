module Advent.Parse
  ( parseIO
  , parseAll
  , token
  , sym
  , twoNewlines
  , module X
  ) where

import Advent.Prelude

import Data.Attoparsec.Text as X hiding (Number(..))

parseIO :: Parser a -> Text -> IO a
parseIO parser = either crash pure . parseAll parser

-- | Parse all input
parseAll :: Parser a -> Text -> Either Text a
parseAll parser = mapLeft pack . parseOnly (parser <* skipSpace <* endOfInput)

-- | Parser that consume trailing whitespace
token :: Parser a -> Parser a
token parser = parser <* skipSpace

-- | Consume and discard a verbatim string
sym :: Text -> Parser ()
sym = void . token . string

-- | Consume two newlines
--
-- Many problem inputs are separated by two newlines
--
twoNewlines :: Parser ()
twoNewlines = endOfLine *> endOfLine
