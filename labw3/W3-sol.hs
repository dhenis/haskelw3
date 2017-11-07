module W3
    where

-- 1. BASIC RECURSION ON LISTS

-- All of the functions in this section are defined using the following pattern: 
-- foo [] = something
-- foo (x:xs) = something else

-- Calculate the length of a list
length' :: [a] -> Int
--Q length' [] = undefined
--Q length' (x:xs) = undefined
length' [] = 0
length' (x:xs) = 1 + length' xs

-- Count the number of zeros in a list
-- countZeros [1,3,0,4,0,1] = 2
countZeros :: [Int] -> Int
--Q countZeros [] = undefined
--Q countZeros (x:xs) = undefined
countZeros [] = 0
countZeros (x:xs) = if x==0 then 1 + countZeros xs else countZeros xs


-- Count the number of even elements (you may want to define an auxiliary function
-- even). 
-- countEven [1,3,0,4,0,1] = 3
countEven :: [Int] -> Int
--Q countEven [] = undefined
--Q countEven (x:xs) = undefined
even' x = mod x 2 == 0
countEven [] = 0
countEven (x:xs) = if even' x then 1 + countEven xs else countEven xs


-- Sum the elements of the list
sum' :: [Int] -> Int
--Q sum' [] = undefined
--Q sum' (x:xs) = undefined
sum' [] = 0
sum' (x:xs) = x + sum' xs

-- Sum the odd elements of the list
sumOdd :: [Int] -> Int
--Q sumOdd [] = undefined
--Q sumOdd (x:xs) = undefined
sumOdd [] = 0
sumOdd (x:xs) = if (odd x) then x + sumOdd xs else sumOdd xs


-- Calculate the product of the elements of the list
product' :: [Int] -> Int
--Q product' [] = undefined
--Q product' (x:xs) = undefined
product' [] = 1
product' (x:xs) = x * product' xs

-- Calculate the product of the non-zero elements of the list
productNonZero :: [Int] -> Int
--Q productNonZero [] = undefined
--Q productNonZero (x:xs) = undefined
productNonZero [] = 1
productNonZero (x:xs) = if x /= 0 then x * productNonZero xs else productNonZero xs

-- Count the number of spaces in a String
spaces :: String -> Int
--Q spaces [] = undefined
--Q spaces (x:xs) = undefined
spaces [] = 0
spaces (x:xs) = if x == ' ' then 1 + spaces xs else spaces xs

-- Take the substring of a String up to the first space character
takeToSpace :: String -> String
--Q takeToSpace [] = undefined
--Q takeToSpace (x:xs) = undefined
takeToSpace [] = []
takeToSpace (x:xs) = if x == ' ' then [] else x:(takeToSpace xs)

-- Take the substring of a String after the first space character
dropToSpace :: String -> String
--Q dropToSpace [] = undefined
--Q dropToSpace (x:xs) = undefined
dropToSpace [] = []
dropToSpace (x:xs) = if x == ' ' then xs else dropToSpace xs

-- 2. MORE COMPLEX RECURSION ON LISTS

-- Check whether a list is sorted into increasing order
isSorted :: [Int] -> Bool
--Q isSorted = undefined
isSorted [] = True
isSorted [x] = True
isSorted (x1:x2:xs) = x1 <= x2 && isSorted (x2:xs)

-- Count the number of strictly increasing pairs
numberStrictlyIncreasingPairs :: [Int] -> Int
--Q numberStrictlyIncreasingPairs = undefined
numberStrictlyIncreasingPairs [] = 0
numberStrictlyIncreasingPairs [x] = 0
numberStrictlyIncreasingPairs  (x1:x2:xs) = 
   if (x1 < x2) then 1 + numberStrictlyIncreasingPairs (x2:xs) 
     else numberStrictlyIncreasingPairs (x2:xs)



-- Count the number of non-strictly increasing pairs
numberIncreasingPairs :: [Int] -> Int
--Q numberIncreasingPairs = undefined
numberIncreasingPairs [] = 0
numberIncreasingPairs [x] = 0
numberIncreasingPairs  (x1:x2:xs) = 
   if (x1 <= x2) then 1 + numberIncreasingPairs (x2:xs) else numberIncreasingPairs (x2:xs)


-- Split a list of characters into sublists at a particular entry
split :: Char -> [Char] -> [[Char]]
split c [] = [[]]
split c (x:xs) = 
  if x==c then []:(split c xs)
  else let (hd:tl) = split c xs in (x:hd):tl

-- 3. MERGE SORT

-- Define a function halve that splits a list into two halves whose 
-- lengths differ by at most one
halve :: [Int] -> ([Int],[Int])
--Q halve [] = undefined
--Q halve [x1] = undefined
--Q halve (x1:x2:xs) = undefined
halve [] = ([],[])
halve [x] = ([x],[])
halve (x1:x2:xs) = let (xs1,xs2) = halve xs in (x1:xs1,x2:xs2)

-- Define a function merge that merges two sorted lists into one sorted list. 
merge :: [Int] -> [Int] -> [Int]
--Q merge xs [] = undefined
--Q merge [] (y:ys) = undefined
--Q merge (x:xs) (y:ys) = undefined
merge xs [] = xs
merge [] (y:ys) = y:ys
merge (x:xs) (y:ys) = if x<y then x:(merge xs (y:ys)) else y:(merge (x:xs) ys)

-- Define a function mergesort that sorts a list
-- mergesort of an empty list is the empty list
-- mergesort of a single element list leaves it unchanged
-- mergesort of a list with at least two elements is obtained by halving the list, 
-- sorting the two halves and merging the results. 
mergesort :: [Int] -> [Int]
--Q mergesort [] = undefined
--Q mergesort [x] = undefined
--Q mergesort (x1:x2:xs) = undefined
mergesort [] = []
mergesort [x] = [x]
mergesort (x1:x2:xs) = let (xs1,xs2) = halve (x1:x2:xs) in 
                        merge (mergesort xs1) (mergesort xs2)


-- 4. ARITHMETICAL RECURSION

-- exponentiation: calculate x**y (x to the power y) by recursion on y, assume y>=0
exp' :: Int -> Int -> Int
exp' x y | y==0 = 1
exp' x y | otherwise = y * (exp' x (y-1))

-- log: calculate log y x (log to the base y of x)
log' :: Int -> Int -> Int
log' y x | x <= y = 0
log' y x | otherwise = 1 + (log' y (div x y))

-- gcd: calculate gcd using recursion: 
-- if x==y then gcd x y is x
-- if x < y then gcd x y is gcd x (y-x)
-- if x > y then gcd x y is gcd y x
gcd' :: Int -> Int -> Int
gcd' x y | x==y = x
gcd' x y | x < y = gcd' x (y-x)
gcd' x y | otherwise = gcd' y x



-- 5. RECURSION ON TREES
data SimpleTree = Leaf Int 
                | Node SimpleTree SimpleTree
                
-- size: the total number of nodes and leaves in a tree
size :: SimpleTree -> Int
size (Leaf n) = 1
size (Node t1 t2) = (size t1) + (size t2) + 1 

-- number of leaves
leaves :: SimpleTree -> Int
leaves (Leaf n) = 1
leaves (Node t1 t2) = (leaves t1) + (leaves t2) 

-- fringe
fringe :: SimpleTree -> [Int]
fringe (Leaf n) = [n]
fringe (Node t1 t2) = (fringe t1) ++ (fringe t2) 

-- height
height :: SimpleTree -> Int
height (Leaf n) = 0
height (Node t1 t2) = max (height t1) (height t2) + 1

-- multiply all leaves by 2
doubleLeaves :: SimpleTree -> SimpleTree
doubleLeaves (Leaf n) = Leaf (2*n)
doubleLeaves (Node t1 t2) = Node (doubleLeaves t1) (doubleLeaves t2) 

   
                
                