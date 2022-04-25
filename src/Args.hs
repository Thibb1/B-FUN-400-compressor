-- Arguments handling module
module Args where

-- Imports
import Text.Read ( readMaybe )
import Types

parser :: Options -> [String] -> Maybe Options
parser res [] = Just res
parser _ [_] = Nothing
parser (Options n l _) ("-f":f:xs) = parser (Options n l f) xs
parser (Options _ l f) ("-n":x:xs) = case readMaybe x of
  Just n -> parser (Options n l f) xs
  Nothing -> Nothing
parser (Options n _ f) ("-l":x:xs) = case readMaybe x of
  Just l -> parser (Options n l f) xs
  Nothing -> Nothing
parser _ _ = Nothing

checkParsed :: Maybe Options -> Maybe Options
checkParsed Nothing = Nothing
checkParsed (Just (Options n l f))
  | n <= 0 = Nothing
  | l < 0 = Nothing
  | null f = Nothing
  | otherwise = Just (Options n l f)

parseArgs :: [String] -> Maybe Options
parseArgs args = checkParsed (parser (Options (-1) (-1) "") args)