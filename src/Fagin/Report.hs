{-| Error and Warning handling.
 -
 - The goal here is to be able to distinguish between Errors, which return no
 - value and terminate immediately, Warnings, which are accumulated.
 -
 -}

module Fagin.Report
(
    Report(..)
  , ReportS
  , ShowE(..)
  , sequenceR
  , pass , pass'
  , fail , fail'
  , stop , stop'
  , warn , warn'
  , note , note'
) where

import Fagin.Prelude

type ReportS = Report [ByteString]

data Report e a
  = Pass !a !e !e
  | Fail !e !e !e
  deriving(Eq,Ord,Show,Generic,NFData)

pass :: (Monoid e) => a -> Report e a
pass x = Pass x mempty mempty

fail :: (Monoid e) => e -> Report e a
fail e = Fail e mempty mempty

stop :: (Monoid e) => e -> a -> Report e a
stop e _ = Fail e mempty mempty

warn :: (Monoid e) => e -> a -> Report e a
warn e x = Pass x e mempty

note :: (Monoid e) => e -> a -> Report e a
note e x = Pass x mempty e

pass' :: a -> Report [e] a
pass' x = Pass x [] []

fail' :: e -> Report [e] a
fail' e = Fail [e] [] []

stop' :: e -> a -> Report [e] a
stop' e _ = Fail [e] [] []

warn' :: e -> a -> Report [e] a
warn' e x = Pass x [e] []

note' :: e -> a -> Report [e] a
note' e x = Pass x [] [e]

instance (Monoid e) => Monad (Report e) where
  return x = Pass x mempty mempty

  (Pass x1 w1 n1) >>= f = case f x1 of
    (Pass x2 w2 n2) -> Pass x2 (w1 ++ w2) (n1 ++ n2)
    (Fail e2 w2 n2) -> Fail e2 (w1 ++ w2) (n1 ++ n2)
  (Fail e w n) >>= _ = (Fail e w n)

instance Functor (Report e) where
  fmap f (Pass x w n) = Pass (f x) w n
  fmap _ (Fail e w n) = Fail e w n

instance (Monoid e) => Applicative (Report e) where
  pure x = Pass x mempty mempty

  Pass f  w1 n1 <*> Pass x  w2 n2 = Pass (f x)      (w1 ++ w2) (n1 ++ n2)
  Pass _  w1 n1 <*> Fail e2 w2 n2 = Fail e2         (w1 ++ w2) (n1 ++ n2)
  Fail e1 w1 n1 <*> Pass _  w2 n2 = Fail e1         (w1 ++ w2) (n1 ++ n2)
  Fail e1 w1 n1 <*> Fail e2 w2 n2 = Fail (e1 ++ e2) (w1 ++ w2) (n1 ++ n2)

sequenceR :: [ReportS a] -> ReportS [a]
sequenceR [] = pass' []
sequenceR ((Fail e w n):_) = Fail e w n
sequenceR ((Pass v w n):rs) = foldr' comb' (Pass [v] w n) rs where 
  comb' :: ReportS a -> ReportS [a] -> ReportS [a]
  comb' (Pass x  w1 n1) (Pass xs w2 n2) = Pass (x:xs)     (w1 ++ w2) (n1 ++ n2)
  comb' (Pass _  w1 n1) (Fail e2 w2 n2) = Fail e2         (w1 ++ w2) (n1 ++ n2)
  comb' (Fail e1 w1 n1) (Pass _  w2 n2) = Fail e1         (w1 ++ w2) (n1 ++ n2)
  comb' (Fail e1 w1 n1) (Fail e2 w2 n2) = Fail (e1 ++ e2) (w1 ++ w2) (n1 ++ n2)

class ShowE e where
  showE :: e -> ByteString

  showError :: e -> ByteString
  showError = showE

  showWarning :: e -> ByteString
  showWarning = showE

  showNote :: e -> ByteString
  showNote = showE

  show3E :: e -> e -> e -> ByteString
  show3E e w n = unlines [showError e, showWarning w, showNote n]

instance ShowE [ByteString] where
  showE = unlines 

  show3E e w n = mconcat
    [
      showError   (map (\x -> "ERROR: "   ++ x) e)
    , showWarning (map (\x -> "WARNING: " ++ x) w)
    , showNote    (map (\x -> "NOTE: "    ++ x) n)
    , summary'
    ]
    where
      ne = bshow $ length e
      nw = bshow $ length w
      nn = bshow $ length n
      summary' = unwords [ne, "error(s),", nw, "warning(s),", nn, "note(s)\n"]
