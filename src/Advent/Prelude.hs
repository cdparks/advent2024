module Advent.Prelude
  ( module X
  , crash
  , forAccum
  , getProgName
  , lookupEnv
  , print
  , readCommaSep
  , readMaybe
  , readIO
  , tshow
  , unwrap
  , minmax
  ) where

import Prelude as X hiding
  ( appendFile
  , getContents
  , getContents
  , getLine
  , getLine
  , head
  , init
  , interact
  , last
  , lines
  , print
  , putStr
  , putStrLn
  , putStrLn
  , readFile
  , readIO
  , tail
  , unlines
  , unwords
  , words
  , writeFile
  , (!!)
  )

import Control.Applicative as X
import Control.Arrow as X ((&&&))
import Control.Monad as X
import Control.Monad.IO.Unlift as X
import Control.Monad.State.Strict as X
import Control.Parallel.Strategies as X
import Data.Bifunctor as X
import Data.Bifoldable as X
import Data.Bitraversable as X
import Data.Bits as X
import Data.Bool as X (bool)
import Data.Char as X
import Data.Either as X
import Data.Either.Extra as X (mapLeft)
import Data.Foldable as X
import Data.Hashable as X
import Data.HashMap.Monoidal as X (MonoidalHashMap)
import Data.HashMap.Strict as X (HashMap)
import Data.HashSet as X (HashSet)
import Data.IntMap.Strict as X (IntMap)
import Data.IntSet as X (IntSet)
import Data.Kind as X (Type, Constraint)
import Data.List as X (sort, sortOn, sortBy, transpose, uncons)
import Data.List.Extra as X (maximumOn, minimumOn, nubOrd)
import Data.List.NonEmpty as X (NonEmpty(..))
import Data.Map.Strict as X (Map)
import Data.Maybe as X
import Data.Ord as X
import Data.Proxy as X (Proxy(..))
import Data.Sequence as X (Seq, (<|), (|>), pattern (:<|), pattern (:|>), pattern Empty)
import Data.Set as X (Set)
import Data.Text as X (Text, lines, pack, split, splitOn, unlines, unpack, unwords, words)
import Data.Text.IO as X
import Data.Time.Clock as X
import Data.Traversable as X
import Data.Vector as X (Vector)
import Data.Void as X (Void)
import Data.Word as X
import Debug.Trace as X
import GHC.Generics as X (Generic)
import Lens.Micro as X
import Numeric.Natural as X
import Safe as X hiding (at)
import qualified System.Environment as Env
import System.FilePath as X
import Test.Hspec as X hiding (example)
import Test.Hspec.QuickCheck as X
import Test.Hspec.Runner as X (hspecWith, defaultConfig, Config, configFormatter)
import Test.Hspec.Api.Formatters.V1 as X (specdoc, progress)
import qualified Text.Read as Read
import UnliftIO.Directory as X
import UnliftIO.Exception as X
import UnliftIO.IORef as X
import UnliftIO.Async as X

-- | foldM with the functional argument last
forAccum :: (Foldable t, Monad m) => s -> t a -> (s -> a -> m s) -> m s
forAccum s t f = foldM f s t

-- | Unwrap a 'Maybe' or crash with message
unwrap :: forall a . Text -> Maybe a -> IO a
unwrap message = maybe (crash message) pure

-- | 'Show' to 'Text'
tshow :: forall a . Show a => a -> Text
tshow = pack . show

-- | Print using 'tshow'
print :: forall a . Show a => a -> IO ()
print = putStrLn . tshow

-- | 'Read' 'Text' to 'Maybe'
readMaybe :: forall a . Read a => Text -> Maybe a
readMaybe = Read.readMaybe . unpack

-- | 'Read' or crash with message
readIO :: forall a . Read a => Text -> Text -> IO a
readIO message = unwrap message . readMaybe

-- | Parse comma-separate data from 'Text'
readCommaSep :: Read a => Text -> [a]
readCommaSep = mapMaybe readMaybe . split (== ',')

-- | Lookup environment variable
lookupEnv :: Text -> IO (Maybe Text)
lookupEnv = fmap (fmap pack) . Env.lookupEnv . unpack

-- | Get program name
getProgName :: IO Text
getProgName = pack <$> Env.getProgName

-- | Crash with message
crash :: Text -> IO a
crash = throwIO . userError . unpack

-- | Calculate 'min' and 'max' together
minmax :: Ord a => a -> a -> (a, a)
minmax x y
  | x <= y = (x, y)
  | otherwise = (y, x)
