module Fagin.IO
(
    writeResult
  , writeResultAndExit
) where

import Fagin.Prelude
import Fagin.Report

import qualified System.Exit as SE
import System.IO (stderr, stdout)
import Control.Monad (mapM_)

writeResult :: (Monoid e, ShowE e, BShow o) => Report e [o] -> IO ()
writeResult (Pass xs w n)
  =  mapM_ (\s -> hPut stdout $ bshow s) xs
  >> hPut stderr (show3E mempty w n)
writeResult (Fail e w n)
  =  hPut stderr (show3E e w n)

writeResultAndExit :: (Monoid e, ShowE e, BShow o) => Report e [o] -> IO a
writeResultAndExit (Pass x w n)
  =  writeResult (Pass x w n)
  >> SE.exitWith SE.ExitSuccess
writeResultAndExit f
  =  writeResult f
  >> SE.exitWith (SE.ExitFailure 1)
