# Recursions
# http://learnyouahaskell.com/recursion


maximum1 :: (Ord a) => [a] -> a
maximum1 [] = error "maximum of empty list"
maximum1 [x] = x
maximum1 (x:xs)
    | x > maxTail = x
    | otherwise = maxTail
    where maxTail = maximum1 xs

#*

maximum2 :: (Ord a) => [a] -> a
maximum2 [] = error "maximum of empty list"
maximum2 [x] = x
maximum2 (x:xs) = max x (maximum2 xs)


#***********************


quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) =  left ++ [x] ++  right
    where left = quickSort [e | e <- xs, e < x]
          right = quickSort [e | e <- xs, e >= x]

#*

quickSort1 :: (Ord a) => [a] -> [a]
quickSort1 [] = []
quickSort1 (x:xs) =
    let left = quickSort1 [e | e <- xs, e < x]
        right = quickSort1 [e | e <- xs, e >= x]
    in left ++ [x] ++  right

#*
quickSort2 :: (Ord a) => [a] -> [a]
quickSort2 [] = []
quickSort2 (p:xs) = quickSort2 lesser ++ [p] ++ quickSort2 greater
    where lesser  = filter (< p) xs
          greater = filter (>= p) xs


#*

quicksort3 :: (Ord a) => [a] -> [a]
quicksort3 [] = []
quicksort3 (x:xs) =
    let smallerSorted = quicksort3 (filter (<=x) xs)
        biggerSorted = quicksort3 (filter (>x) xs)
    in  smallerSorted ++ [x] ++ biggerSorted


#*************************
replicate1 :: (Num i, Ord i) => i -> a -> [a]
replicate1 0 x = []
replicate1 1 x = [x]
replicate1 n x = x:replicate1 (n-1) x

#*

replicate2 :: (Num i, Ord i) => i -> a -> [a]
replicate2 n x
    | n <= 0    = []
    | otherwise = x:replicate2 (n-1) x

#****************************

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0   = []
take' _ []     = []
take' n (x:xs) = x : take' (n-1) xs

#*

take1 :: (Num i, Ord i) => i -> [a] -> [a]
take1 _ [] = []
take1 0 _ = []
take1 n (x:xs) = x : take1 (n-1) xs

#**************
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

#**********************

repeat' :: a -> [a]
repeat' x = x:repeat' x

#*************************

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

#*********************************


elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
    | a == x    = True
    | otherwise = a `elem'` xs


#**********************

-- sum1 [] = 0
sum1 [x] = x
sum1 (x : xs) = x + sum1 xs

# High order functions

#***********************
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys


#***********************

flip1 :: (a -> b -> c) -> (b -> a -> c)
flip1 f = g
    where g x y = f y x


#*

flip2 :: (a -> b -> c) -> b -> a -> c
flip2 f y x = f x y


#********************************

map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs

#*********************

filter1 :: (a -> Bool) -> [a] -> [a]
filter1 _ [] = []
filter1 f x = [e | e <- x, f e == True]

filter2 :: (a -> Bool) -> [a] -> [a]
filter2 _ [] = []
filter2 p (x:xs)
    | p x       = x : filter2 p xs
    | otherwise = filter2 p xs


#***************************
largestDivisible1 :: (Integral a) => a
largestDivisible1 x n = head [e | e <- [x, x-1..], e `mod` n == 0]

#*
largestDivisible2 :: (Integral a) => a
largestDivisible2 x n = head (filter f [x, x-1..])
    where
        f e = e `mod` n == 0


#***************************

--find the sum of all odd squares that are smaller than 10,000

sum (takeWhile (<10000) (filter odd (map (^2) [1..])))

#*

sum (takeWhile (<10000) [n^2 | n <- [1..], odd (n^2)])


#***************************

--takeWhile Implement!

#*************************

{-
For our next problem, we'll be dealing with Collatz sequences. We take a natural number.
If that number is even, we divide it by two. If it's odd, we multiply it by 3 and then add 1 to that.
We take the resulting number and apply the same thing to it, which produces a new number and so on.
In essence, we get a chain of numbers. It is thought that for all starting numbers, the chains finish
at the number 1. So if we take the starting number 13, we get this sequence:
13, 40, 20, 10, 5, 16, 8, 4, 2, 1. 13*3 + 1 equals 40.
40 divided by 2 is 20, etc. We see that the chain has 10 terms.
-}


chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
    | even n =  n:chain (n `div` 2)
    | odd n  =  n:chain (n*3 + 1)


#**********************************************
-- Types and typeclasses


#*************************
-- Binary search tree
--stud

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
    | x == a = Node x left right
    | x < a  = Node a (treeInsert x left) right
    | x > a  = Node a left (treeInsert x right)


treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
    | x == a = True
    | x < a  = treeElem x left
    | x > a  = treeElem x right

main = print(result)
    where
        tree = treeInsert 5 $ treeInsert 1 $ treeInsert 3 $ singleton 2
        result = treeElem 1 tree