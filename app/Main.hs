module Main where

import Lib
import Args
import Compressor

import System.Environment ( getArgs )
import System.Exit ( exitSuccess )

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> throw usage
    _ -> case parseArgs args of
      Just parsedArgs -> imageCompressor parsedArgs
      Nothing -> throw usage
