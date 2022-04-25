module Compressor where


import Lib
import Types
import System.Random (newStdGen)

imageCompressor :: Options -> IO ()
imageCompressor (Options n l f) = do
  seed <- newStdGen
  file <- fileMaybe f
  case file of
    Nothing -> throw "File not found"
    Just file -> case readPixels file of
      Nothing -> throw "File not supported"
      -- display all the pixels with showPixels ([String])
      Just pixels -> putStr $ unlines $ showPixels pixels