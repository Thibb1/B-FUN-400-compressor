module Lib where

import Control.Exception ( catch )
import System.IO ( stderr, hPutStrLn )
import System.Exit ( exitWith, ExitCode(ExitFailure) )
import Text.Read ( readMaybe )
import Types

-- Read a file and return the contents as a string
fileMaybe :: String -> IO (Maybe String)
fileMaybe file = (Just <$> readFile file) `catch` outputMaybe
  where
    outputMaybe :: IOError -> IO (Maybe String)
    outputMaybe _ = return Nothing

-- Exit with error message
throw :: String -> IO ()
throw msg = hPutStrLn stderr msg >> exitWith (ExitFailure 84)

-- Usage message
usage = "USAGE: ./imageCompressor -n N -l L -f F\n\n\tN\tnumber of colors in"
  ++ " the final image\n\tL\tconvergence limit\n\tF\tpath to the file "
  ++ "containing the colors of the pixels"

-- get the list of pixels from a string
readPixels :: String -> Maybe [Pixel]
readPixels str = mapM readPixel (lines $ filter (not . flip elem "()") str)

-- get a pixel from a string
readPixel :: String -> Maybe Pixel
readPixel str = do
  case words str of
    [pos,color] -> do
      [x,y] <- intList pos
      [r,g,b] <- floatList color
      return (newPixel (newCoords x y) (newColor r g b))
    _ -> Nothing

-- get a list of ints from a string comma separated '1,2,3,...'
intList :: String -> Maybe [Int]
intList str = sequence (readMaybe <$> stringList str)

floatList :: String -> Maybe [Float]
floatList str = sequence (readMaybe <$> stringList str)

-- get a list of strings from a comma separated string
stringList :: String -> [String]
stringList str = words [if x == ',' then ' ' else x | x <- str]

-- print a list of strings
printList :: [String] -> IO ()
printList = mapM_ putStrLn