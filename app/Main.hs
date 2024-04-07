
import Data.Char(ord, toUpper)

------------------------- a

allTrue1 :: [Bool] -> Bool
allTrue1 [] = True
allTrue1 (x:xs) = x && allTrue1 xs

allTrue2 :: [Bool] -> Bool
allTrue2 = and

allTrue3 :: [Bool] -> Bool
allTrue3 = foldr (&&) (True) 

allTrue4 :: [Bool] -> Bool
allTrue4 = foldl (&&) True


------------------------- b

longestLength1 :: [[a]] -> Int
longestLength1 [x] = length x
longestLength1 (x:xs) = max (length x)(longestLength1 xs)

longestLength2 :: [[a]] -> Int
longestLength2 xs = maximum [length x | x <- xs ] 

longestLength3 :: [[a]] -> Int
longestLength3 = foldr1 max . map length 

longestLength4 :: [[a]] -> Int
longestLength4 = foldl1 max . map length 

------------------------- c

sumOddSquares :: [Int] -> Int
sumOddSquares [] = 0
sumOddSquares (x : xs) 
  |odd x = x^2 + sumOddSquares xs
  |otherwise = sumOddSquares xs

sumOddSquares2 :: [Int] -> Int
sumOddSquares2 xs = sum [x ^ 2 | x <- xs , odd x]

sumOddSquares3 :: [Int] -> Int
sumOddSquares3 = foldr (+)(0) . map (^2). filter odd 

sumOddSquares4 :: [Int] -> Int
sumOddSquares4 = foldr ((+) . (^2))(0) .  filter odd 

sumOddSquares5 :: [Int] -> Int
sumOddSquares5 = foldl (+)(0) . map (^2). filter odd 


------------------------- d

shortFWords :: [String] -> Bool
shortFWords [] = False
shortFWords (x : xs) 
  | length x == 4 , head x == 'F' = True
  | otherwise = shortFWords xs

shortFWords2 :: [String] -> Bool
shortFWords2 xs = or  [head x == 'F' | x <- xs , length x == 4]

shortFWords5 :: [String] -> Bool
shortFWords5 = foldr ((||).(=='F').head) False . filter ((== 4) . length)
------------------------- e

wordScore :: String -> Int
wordScore [] = 0
wordScore (x:xs) = score  + wordScore xs
  where 
    score  = subtract 64 (ord(toUpper x))


wordScore2 :: String -> Int
wordScore2 xs = sum[subtract 64 (ord(toUpper x)) | x <- xs]

wordScore3 :: String -> Int
wordScore3 = foldr (+) (0) . filter (1 <=) . filter (<= 26) .  map (subtract 64 . ord .  toUpper)
------------------------- f

concatCheapWords :: [String] -> String
concatCheapWords [] = []
concatCheapWords (x:xs)
  | score <= 42 = " " ++ x ++ concatCheapWords xs
  | otherwise = concatCheapWords xs
  where
    score = wordScore3 x

    
concatCheapWords2 :: [String] -> String
concatCheapWords2 xs = concat [" " ++ x | x <- xs , wordScore3 x  <= 42]

concatCheapWords3 :: [String] -> String 
concatCheapWords3 = foldl (++) [] . map (' ':).filter ((<= 42).wordScore3)
  