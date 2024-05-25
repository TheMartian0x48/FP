{-# LANGUAGE FlexibleInstances, UndecidableInstances, DuplicateRecordFields #-}

module Main where

import Control.Monad
import Data.Array
import Data.Bits
import Data.List
-- import Data.List.Split
import Data.Set
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe

-- f :: IO()
f 0 = do
    return ()
f n = do
    putStrLn "Hello World"
    f (n - 1)


main :: IO()
main = do
    n <- readLn :: IO Int
    f n
    -- Print "Hello World" on a new line 'n' times.

