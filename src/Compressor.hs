module Compressor where


import Lib
import Types
import System.Random (newStdGen)

imageCompressor :: Options -> IO ()
imageCompressor (Options n l f) = do
  seed <- newStdGen
  file <- readMaybe f
  case file of
    Nothing -> throw "File not found"
    Just file -> putStrLn file