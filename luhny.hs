module Luhny where

import Data.Char (digitToInt, isDigit)
import Data.List (cycle, nub, sortBy)
import Data.Ord (comparing)
import Text.Regex.TDFA


main :: IO ()
main = catch loop done
    where done = const $ return ()
          loop = getLine >>= (putStrLn . mask_cc) >> loop


mask_cc :: String -> String
mask_cc original = apply_replacements original sorted_replacements
    where candidates = (nub $ concat (original =~ "([0-9 -]+)" :: [[String]]))
          cleanup = (map find_valid_subsequences) . strip_short_sequences . strip_safe_chars
          replacements = concat $ map filter_luhn $ cleanup candidates
          sorted_replacements = sortBy (comparing (\ x -> length x * (-1))) replacements


-- Return only the numeric characters in a string.
strip_safe_chars :: [String] -> [String]
strip_safe_chars = map $ filter isDigit


-- Strip numeric sequences that are too short
strip_short_sequences :: [String] -> [String]
strip_short_sequences = filter (\ x -> length x > 13)


-- Find all subsequences of 14, 15, or 16 numbers *or* large groups of zeros.
find_valid_subsequences :: String -> [String]
find_valid_subsequences cs = concat all_subsequences
    where all_subsequences = [ordered_subsequences i cs | i <- [14 .. 16]]


ordered_subsequences :: Int -> [Char] -> [[Char]]
ordered_subsequences len cs
    | all_zeros cs = [cs]
    | length cs < len = []
    | length cs == len = [cs]
    | length cs > len = subcs : (ordered_subsequences len (tail cs))
    where all_zeros = all ('0' ==)
          subcs = take len cs


-- Recursively mask a list of luhn numbers in a string.
apply_replacements :: String -> [String] -> String
apply_replacements line [] = line
apply_replacements line (r : rs) = apply_replacements masked_line rs
    where masked_line = mask line r


-- Does number represented in this string pass the Luhn check?
luhn_check :: String -> Bool
luhn_check xs = (sum $ double_alternates $ map digitToInt xs) `mod` 10 == 0


-- Return only strings from a list that pass the Luhn check.
filter_luhn :: [String] -> [String]
filter_luhn = filter luhn_check


-- Double integer. If a product has two digits, treat the digits independently.
double_and_split_product :: Int -> [Int]
double_and_split_product n = map digitToInt $ show p
    where p = n * 2


-- Taking a list of integers and starting from the *rightmost* digit
-- and working left, double every second digit.
double_alternates :: [Int] -> [Int]
double_alternates ns = concat $ map dbl alternating_ns
    where bs = cycle $ if odd $ length ns then [False, True] else [True, False]
          alternating_ns = zip ns bs
          dbl (n, b) = if b then double_and_split_product n else [n]


-- Given a string and a substring, if the string contains the substring
-- Then mask the substring chracters in the string with "X", ignoring
-- dashes and spaces while consuming the string.
mask :: String -> String -> String
mask str repl
  | repl_length > length str = str
  | repl_length == 0 = str
  | otherwise = _mask str repl "" "" repl
  where repl_length = length repl


_mask :: String -> String -> String -> String -> String -> String
_mask str [] _ out m = out ++ str -- sub is exhausted: match made.
_mask [] _ acc _ _ = acc -- str is exhausetd: no match made.
_mask (s : xs) (u : us) acc out m
  | s == u = _mask xs us (acc ++ [s]) (out ++ "X") m
  | is_safe_char s = _mask xs (u : us) (acc ++ [s]) (out ++ [s]) m
  | otherwise = _mask xs m (acc ++ [s]) (acc ++ [s]) m
  where is_safe_char c = c == ' ' || c == '-'
