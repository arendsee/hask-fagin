module Fagin.Interval (
    Strand(..)
  , Interval(..)
) where

import Fagin.Prelude

data Strand
  = Plus
  | Minus
  deriving(Ord,Eq,Show,Generic,NFData)

instance BShow Strand where
  bshow Plus  = "+"
  bshow Minus = "-"

data Interval
  = Interval
    !Integer
    !Integer
  deriving(Ord,Eq,Show,Generic,NFData)

instance Semigroup Interval where
  (Interval a1 b1) <> (Interval a2 b2) = Interval (min a1 a2) (max b1 b2) 
