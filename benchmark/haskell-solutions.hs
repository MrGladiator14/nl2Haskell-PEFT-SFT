-- Answer 1: Module declaration
module Anagram (anagramsFor) where

-- Answer 2: Import statements
import Data.List
import Data.Char

-- Answer 3: Lower function
lower :: String -> String
lower = map toLower

-- Answer 4: Normalize function
normalize :: String -> String
normalize = sort . lower

-- Answer 5: Anagram function
anagram :: String -> String -> Bool
anagram x y = normalize x == normalize y

-- Answer 6: AnagramsFor function
anagramsFor :: String -> [String] -> [String]
anagramsFor xs = filter (\x -> x /= xs && anagram x xs)

-- Answer 7: Clock module declaration
module Clock (addDelta, fromHourMin, toString) where

-- Answer 8: Import printf
import Text.Printf

-- Answer 9: Clock data type
data Clock = Clock { dayMins :: Int } deriving (Eq)

-- Answer 10: fromHourMin function
fromHourMin :: Int -> Int -> Clock
fromHourMin hour minute = Clock $ mod (hour * 60 + minute) (24 * 60)

-- Answer 11: toString function
toString :: Clock -> String
toString (Clock mins) = printf "%02d:%02d" hours minutes
  where
    hours = mod (div mins 60) 24
    minutes = mod mins 60

-- Answer 12: addDelta function
addDelta :: Int -> Int -> Clock -> Clock
addDelta hour minute (Clock mins) = 
    Clock $ mod (mins + hour * 60 + minute) (24 * 60)

-- Answer 13: Hamming module
module Hamming (distance) where

-- Answer 14: Distance type signature
distance :: String -> String -> Maybe Int

-- Answer 15: Distance implementation
distance xs ys
    | length xs /= length ys = Nothing
    | otherwise = Just $ length $ filter (uncurry (/=)) $ zip xs ys

-- Answer 16: Luhn module
module Luhn (isValid) where

-- Answer 17: Import Data.Char
import Data.Char

-- Answer 18: Double function
double :: String -> String
double s = construct (reverse s) 1

-- Answer 19: Construct helper function
construct :: String -> Int -> String
construct [] _ = []
construct (x:xs) n
    | n `mod` 2 == 0 = let doubled = digitToInt x * 2
                       in if doubled > 9 
                          then show (doubled - 9) ++ construct xs (n+1)
                          else show doubled ++ construct xs (n+1)
    | otherwise = x : construct xs (n+1)

-- Answer 20: SumS function
sumS :: String -> Int
sumS = sum . map digitToInt

-- Answer 21: IsValid function
isValid :: String -> Bool
isValid s = let normalized = filter isDigit s
            in length normalized > 1 && 
               normalized /= "0" && 
               sumS (double normalized) `mod` 10 == 0

-- Answer 22: Nth prime type signature
nth :: Int -> Maybe Integer

-- Answer 23: Nth implementation
nth n
    | n < 1 = Nothing
    | n == 1 = Just 2
    | otherwise = Just $ primes !! (n-1)
    where primes = 2 : filter isPrime [3,5..]

-- Answer 24: IsPrime function
isPrime :: Integer -> Bool
isPrime i = i > 1 && all (\x -> i `mod` x /= 0) [2..floor $ sqrt $ fromIntegral i]

-- Answer 25: GameOfLife module
module GameOfLife (tick) where

-- Answer 26-30: Tick implementation
tick :: [[Int]] -> [[Int]]
tick grid = [[turnCell x y | x <- indices] | y <- indices]
  where
    indices = [0..length grid - 1]
    
    turnCell x y = case grid !! y !! x of
        0 -> if countLive x y == 3 then 1 else 0
        1 -> if countLive x y `elem` [2,3] then 1 else 0
    
    countLive x y = sum [grid !! ny !! nx | 
        nx <- [x-1..x+1], 
        ny <- [y-1..y+1],
        nx >= 0, nx < length grid,
        ny >= 0, ny < length grid,
        not (nx == x && ny == y)]

-- Answer 31: Triangle module
module Triangle (rows) where

-- Answer 32: Rows type signature
rows :: Int -> [[Integer]]

-- Answer 33: Rows implementation
rows n = take n pascal
  where
    pascal = [1] : map nextRow pascal
    nextRow row = zipWith (+) (0:row) (row ++ [0])
