-- Arguments handling module
module Args where

-- Imports
import System.IO ( stderr, hPutStrLn )
import System.Exit ( exitWith, ExitCode(ExitFailure) )
import Text.Read ( readMaybe )
import Types

-- Exit with error message
throw :: String -> IO ()
throw msg = hPutStrLn stderr msg >> exitWith (ExitFailure 84)

usage = "USAGE: ./imageCompressor -n N -l L -f F\n\n\tN\tnumber of colors in"
  ++ " the final image\n\tL\tconvergence limit\n\tF\tpath to the file "
  ++ "containing the colors of the pixels"

parser :: InputArgs -> [String] -> Maybe InputArgs
parser res [] = Just res
parser _ [_] = Nothing
parser (InputArgs n l _) ("-f":f:xs) = parser (InputArgs n l f) xs
parser (InputArgs _ l f) ("-n":x:xs) = case readMaybe x of
  Just n -> parser (InputArgs n l f) xs
  Nothing -> Nothing
parser (InputArgs n _ f) ("-l":x:xs) = case readMaybe x of
  Just l -> parser (InputArgs n l f) xs
  Nothing -> Nothing
parser _ _ = Nothing

checkParsed :: Maybe InputArgs -> Maybe InputArgs
checkParsed Nothing = Nothing
checkParsed (Just (InputArgs n l f))
  | n <= 0 = Nothing
  | l < 0 = Nothing
  | null f = Nothing
  | otherwise = Just (InputArgs n l f)

parseArgs :: [String] -> Maybe InputArgs
parseArgs args = checkParsed (parser (InputArgs (-1) (-1) "") args)