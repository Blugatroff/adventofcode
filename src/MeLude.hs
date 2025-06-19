module MeLude (module X) where

import Control.Applicative as X (Applicative (..), (<|>))
import Control.Arrow as X ((>>>))
import Control.Monad as X (
  Monad (..),
  foldM,
  foldM_,
  forever,
  guard,
  replicateM,
  replicateM_,
  unless,
  when,
  zipWithM,
  zipWithM_,
  (<=<),
  (=<<),
  (>=>),
 )
import Control.Monad.ST as X (ST, runST)
import Control.Monad.State as X (MonadState)
import Data.Array.Base as X (IArray (..), UArray, array, (!), (//))
import Data.Bifunctor as X (first, second)
import Data.Bool as X
import Data.Char as X (Char, isAlpha, isAsciiLower, isAsciiUpper, isDigit, isLower, isSpace, isUpper, ord)
import Data.Either as X
import Data.Either.Extra as X
import Data.Eq as X
import Data.Foldable as X
import Data.Function as X (const, flip, id, on, ($), (&), (.))
import Data.Functor as X (Functor (..), void, ($>), (<$), (<$>), (<&>))
import Data.Int as X
import Data.Ix as X
import Data.List as X (
  cycle,
  drop,
  dropWhile,
  dropWhileEnd,
  elemIndex,
  filter,
  findIndex,
  head,
  uncons,
  unsnoc,
  init,
  intercalate,
  intersperse,
  isPrefixOf,
  iterate,
  last,
  map,
  nub,
  repeat,
  replicate,
  reverse,
  singleton,
  sort,
  sortBy,
  span,
  splitAt,
  tail,
  take,
  takeWhile,
  transpose,
  zip,
  zipWith,
  (!!),
  (++),
 )
import Data.List.Extra as X (dropEnd, firstJust)
import Data.List.NonEmpty as X (NonEmpty (..))
import Data.Map as X (Map)
import Data.Maybe as X
import Data.Monoid as X (Monoid (..))
import Data.Ord as X (Ord (..), Ordering (..))
import Data.Semigroup as X (Semigroup (..))
import Data.Set as X (Set)
import Data.String as X
import Data.Traversable as X
import Data.Tuple as X (fst, snd, swap, uncurry)
import Data.Void as X (Void, absurd)
import Data.Word as X (Word8)
import System.IO as X (IO, getContents, hPrint, hPutStr, hPutStrLn, print, putStr, putStrLn, readFile, stderr, stdin, stdout, writeFile)
import System.IO.Error as X (IOError)
import Text.Read as X (Read (..), readEither)
import Text.Show as X (Show (..))
import Prelude as X (
  Bounded (..),
  Double,
  Enum (..),
  Float,
  Floating,
  Integer,
  Integral (..),
  Num (..),
  ceiling,
  error,
  even,
  fromIntegral,
  gcd,
  logBase,
  subtract,
  undefined,
  (^),
 )
