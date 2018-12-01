{-# LANGUAGE NoImplicitPrelude #-}

module Main(
  main
) where

import Data.Aviation.Casr.Logbook
import System.Environment(getArgs)
import System.IO(IO, putStrLn)

main ::
  IO ()
main =
  do  a <- getArgs
      putStrLn "hi"
