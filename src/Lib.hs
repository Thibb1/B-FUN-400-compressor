module Lib where

import Control.Exception (catch)
import System.IO ( stderr, hPutStrLn )
import System.Exit ( exitWith, ExitCode(ExitFailure) )

-- Read a file and return the contents as a string
readMaybe :: String -> IO (Maybe String)
readMaybe file = (Just <$> readFile file) `catch` outputMaybe
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

--you should read the list of pixels from a file passed as argument, according to the following grammar:
--IN ::= POINT ' ' COLOR ( '\n ' POINT ' ' COLOR ) *
--POINT ::= '( ' int ',' int ') '
--COLOR ::= '( ' SHORT ',' SHORT ',' SHORT ') '
--SHORT ::= '0 '.. '

--readPixels :: String -> [(Int, Int, Int)]
--readPixels file =
--    let
--        IN = do
--            point <- readPoint
--            color <- readColor
--            return (point, color)
--        readPoint = do
--            x <- readInt
--            y <- readInt
--            return (x, y)
--        readColor = do
--            r <- readShort
--            g <- readShort
--            b <- readShort
--            return (r, g, b)
--        readShort = do
--            s <- read
--            return (read s :: Int)
