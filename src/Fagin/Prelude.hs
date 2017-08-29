{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}

module Fagin.Prelude (
    module CorePrelude
  , module Safe
  , module Control.DeepSeq
  , Generic
  , ConstantString
  -- monoid operator
  , (++)
  -- semigroup (with <> operator)
  , Semigroup(..) 
  , BShow(..)
  , Sequence(..)
  , P.show
  , DL.filter
  , DL.find
  , DL.zip
  , DL.sort
  , DL.group
  , DL.lookup
  , map
  , DL.concatMap
  -- Control
  , CM.sequence
  , CM.mapM
  , DF.fold
  , DF.foldr
  , DF.foldr'
  , DF.foldl
  , DF.foldl'
  -- ShortByteString
  , DBS.ShortByteString
  , DBS.toShort
  , DBS.fromShort
  -- ByteString IO
  , print
  , DBC.ByteString
  , DBC.readFile
  , DBC.writeFile
  , DBC.appendFile
  , DBC.getContents
  , DBC.putStr
  , DBC.putStrLn
  , DBC.interact
  , DBC.hGet
  , DBC.hGetContents
  , DBC.hPut
  , DBC.hPutStr
  , DBC.hPutStrLn
  -- ByteString textual functions
  , DBC.split
  ,     unsplit
  , DBC.words
  , DBC.unwords
  , DBC.lines
  , DBC.unlines
  , DBC.pack
  , DBC.unpack
) where

{-|

For Prelude, I will use the CorePrelude, which is a fairly minimal Prelude that
has most of what I want. The main change I add is to hide the Text functions
and import Data.BytesString.Char8. Effectively this allows ByteString to act as
the primary String class. 

'ByteString' uses half the memory of 'Text' at the cost of handling only ASCII
characters. This is a reasonable tradeoff here since memory is likely to be
limiting and the data are nearly guaranteed to be only ASCII.

I also hide 'fail' from standard Prelude. I do this because I redefine it (like
a fool) in Fagin.Report.

-}

import CorePrelude hiding (
    -- hide Text functions
      putStr   
    , putStrLn
    , getArgs
    , terror
    -- hide strict bytestrings
    , ByteString
    -- hide String functions
    , print
    -- hide unsafe functions
    , fail
    , (<>)
    )

import Safe (headMay)

import Control.DeepSeq
import GHC.Generics (Generic)

import qualified Prelude as P
import qualified Data.ByteString.Char8 as DBC
import qualified Data.ByteString.Short as DBS
import qualified Data.List as DL
import qualified Control.Monad as CM
import qualified Data.Foldable as DF

type ConstantString = DBS.ShortByteString

class Semigroup a where
  infixr 6 <>
  (<>) :: a -> a -> a

infixr 6 ++
(++) :: Monoid a => a -> a -> a
(++) = mappend

class (Monoid f, P.Integral i) => Sequence f e i | f -> e i where
  length      :: f -> i
  reverse     :: f -> f
  intersperse :: e -> f -> f
  transpose   :: [f] -> [f]
  replicate   :: i -> e -> f
  takeWhile   :: (e -> Bool) -> f -> f
  dropWhile   :: (e -> Bool) -> f -> f
  concat      :: [f] -> f
  isPrefixOf  :: f -> f -> Bool
  isSuffixOf  :: f -> f -> Bool
  take        :: i -> f -> f
  drop        :: i -> f -> f
  uncons      :: f -> Maybe (e,f)

instance Sequence DBC.ByteString Char Int where
  length      = DBC.length
  reverse     = DBC.reverse
  intersperse = DBC.intersperse
  transpose   = DBC.transpose
  replicate   = DBC.replicate
  takeWhile   = DBC.takeWhile
  dropWhile   = DBC.dropWhile
  concat      = DBC.concat     
  isPrefixOf  = DBC.isPrefixOf
  isSuffixOf  = DBC.isSuffixOf
  take        = DBC.take
  drop        = DBC.drop
  uncons      = DBC.uncons

instance (Eq a) => Sequence [a] a Int where
  length      = DL.length
  reverse     = DL.reverse
  intersperse = DL.intersperse
  transpose   = DL.transpose
  replicate   = DL.replicate
  takeWhile   = DL.takeWhile
  dropWhile   = DL.dropWhile
  concat      = DL.concat     
  isPrefixOf  = DL.isPrefixOf
  isSuffixOf  = DL.isSuffixOf
  take        = DL.take
  drop        = DL.drop
  uncons      = DL.uncons

map :: (Functor f) => (a -> b) -> f a -> f b
map = P.fmap

unsplit :: Char -> [DBC.ByteString] -> DBC.ByteString
unsplit c = DBC.intercalate (DBC.singleton c)

-- Set default IO to ByteString

print :: (BShow a) => a -> IO ()
print = DBC.putStrLn . bshow 

class (Show a) => BShow a where
  bshow :: a -> DBC.ByteString
  bshow = DBC.pack . P.show

instance BShow Int

instance BShow Integer

instance BShow String where
  bshow = DBC.pack

instance BShow DBC.ByteString where
  bshow = id

instance BShow DBS.ShortByteString where
  bshow = DBS.fromShort

instance BShow a => BShow [a] where
  bshow = DBC.unlines . map bshow
