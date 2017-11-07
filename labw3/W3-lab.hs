module W3
    where

-- 1. BASIC RECURSION ON LISTS

-- All of the functions in this section are defined using the following pattern:
-- foo [] = something
-- foo (x:xs) = something else

-- Calculate the length of a list
length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs

-- Count the number of zeros in a list
-- countZeros [1,3,0,4,0,1] = 2
countZeros :: [Int] -> Int
countZeros [] = 0
countZeros (x:xs) = (if (x==0) then 1 else 0 ) + countZeros xs


-- Count the number of even elements (you may want to define an auxiliary function
-- even').
-- countEven [1,3,0,4,0,1] = 3
even' :: Int -> Bool
even' x = undefined
countEven :: [Int] -> Int
countEven [] = 0
countEven (x:xs) = (if(mod x 2==0) then 1 else 0)+ countEven xs


-- Sum the elements of the list
sum' :: [Int] -> Int
sum' [] = 0
sum' (x:xs) = x + sum' xs

-- Sum the odd elements of the list
sumOdd :: [Int] -> Int
sumOdd [] = 0
sumOdd (x:xs) = (if(mod x 2 == 0) then x  else 0) + sumOdd xs


-- Calculate the product of the elements of the list
product' :: [Int] -> Int
product' [] = 1
product' (x:xs) = x * product' xs

-- Calculate the product of the non-zero elements of the list
productNonZero :: [Int] -> Int
productNonZero [] = 1
productNonZero (x:xs) = (if(x /= 0) then x else 1) * productNonZero xs

-- Count the number of spaces in a String
spaces :: String -> Int
spaces [] = 0
spaces (x:xs) = if x == ' ' then 1 + spaces xs else spaces xs

-- Take the substring of a String up to the first space character
takeToSpace :: String -> String
takeToSpace [] = []
takeToSpace (x:xs) = if x == ' ' then [] else x:(takeToSpace xs)

-- Take the substring of a String after the first space character
dropToSpace :: String -> String
dropToSpace [] = []
dropToSpace (x:xs) = if x == ' ' then xs else dropToSpace xs

-- 2. MORE COMPLEX RECURSION ON LISTS

-- Check whether a list is sorted into increasing order
isSorted :: [Int] -> Bool
--isSorted = undefined
isSorted [] = True
isSorted [x] = True
isSorted (x1:x2:xs) = x1 <= x2 && isSorted (x2:xs)



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
split c (x:xs) = if x==c then []:(split c xs)
                  else let (hd:tl) = split c xs in (x:hd):tl


-- 3. MERGE SORT

-- Define a function halve that splits a list into two halves whose
-- lengths differ by at most one
halve :: [Int] -> ([Int],[Int])
halve [] = ([],[])
halve [x] = ([x],[])
halve (x1:x2:xs) = let (xs1,xs2) = halve xs in (x1:xs1, x2:xs2)

-- Define a function merge that merges two sorted lists into one sorted list.
merge :: [Int] -> [Int] -> [Int]
merge xs [] = xs
merge [] (y:ys) = y:ys
merge (x:xs) (y:ys) = if x<y then x:(merge xs (y:ys)) else y:(merge (x:xs) ys)

-- Define a function mergesort that sorts a list  mulai dari sini
-- mergesort of an empty list is the empty list
-- mergesort of a single element list leaves it unchanged
-- mergesort of a list with at least two elements is obtained by halving the list,
-- sorting the two halves and merging the results.
mergesort :: [Int] -> [Int]
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
log' y x | x <= y =0
log' y x | otherwise = y * (exp' x (y-1))

-- gcd: calculate gcd using recursion:
-- if x==y then gcd x y is x
-- if x < y then gcd x y is gcd x (y-x)
-- if x > y then gcd x y is gcd y x
gcd' :: Int -> Int -> Int
gcd' x y = undefined


-- 5. RECURSION ON TREES
data SimpleTree = Leaf Int
                | Node SimpleTree SimpleTree

-- size: the total number of nodes and leaves in a tree
size :: SimpleTree -> Int
size (Leaf n) = 1
size (Node t1 t2) = (size t1) +(size t2)+1

-- number of leaves
leaves :: SimpleTree -> Int
leaves (Leaf n) = 1
leaves (Node t1 t2) = (leaves t1) + (leaves t2)

-- fringe
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
