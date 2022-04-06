module Lib where

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
