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



#*****************************

import Data.Char
import Data.List

main = do line <- fmap (intersperse '-' . reverse . map toUpper) getLine
          putStrLn line

#***

import Data.Char
import Data.List

main = do line <- fmap (\xs -> intersperse '-' (reverse (map toUpper xs))) getLine
          putStrLn line




#**********************************************
-- Types and typeclasses


#*************************
-- Binary search tree
-- stud

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

-- data means that we're defining a new data type.
-- The part before the = denotes the type, which is Tree. In that case that's type constructor
-- because it has a.
-- Type would be Tree Int for example
-- The parts after the = are value constructors.

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

#*****************************
-- stud

data List a = Empty | Cons { listHead :: a, listTail :: List a} deriving (Show, Read, Eq, Ord)

main = print(a)
    where
        a = Cons 3 (Cons 4 (Cons 5 Empty))
-- Cons {listHead = 3, listTail = Cons {listHead = 4, listTail = Cons {listHead = 5, listTail = Empty}}}

#***



data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)


{-
When we define functions as operators, we can use that to give them a fixity
(but we don't have to). A fixity states how tightly the operator binds and whether it's
left-associative or right-associative. For instance, *'s fixity is infixl 7 * and +'s fixity
is infixl 6. That means that they're both left-associative (4 * 3 * 2 is (4 * 3) * 2) but * binds
 tighter than +, because it has a greater fixity, so 5 * 4 + 3 is (5 * 4) + 3.
 -}

infixr 5 :-:
infixr 5  .++
(.++) :: List a -> List a -> List a
Empty .++ ys = ys
(x :-: xs) .++ ys = x :-: (xs .++ ys)

main = print(c)
    where
        a = 3 :-: 4 :-: 5 :-: Empty
        b = 6 :-: 7 :-: Empty
        c = a .++ b


-- 3 :-: (4 :-: (5 :-: (6 :-: (7 :-: Empty))))


#*********************************

data TrafficLight = Red | Yellow | Green


class Eq1 a where
    (===) :: a -> a -> Bool
    (/==) :: a -> a -> Bool


instance Eq1 TrafficLight where --like deriving Eq1
    Red === Red = True
    Green === Green = True
    Yellow === Yellow = True
    _ === _ = False

instance Show TrafficLight where
    show Red = "Red light"
    show Yellow = "Yellow light"
    show Green = "Green light"


main = print(a)
    where
        a = Red === Red

#*********************************

class YesNo a where
    yesno :: a -> Bool

instance YesNo Int where
    yesno 0 = False
    yesno _ = True


instance YesNo [a] where
    yesno [] = False
    yesno _ = True


instance YesNo Bool where
    yesno = id

instance YesNo (Maybe a) where
    yesno (Just _) = True
    yesno Nothing = False


yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf yesnoVal yesResult noResult = if yesno yesnoVal then yesResult else noResult

main = print(a)
    where
        a = yesnoIf [] "YEAH!" "NO!"


# IO
#****************************************

putStr :: String -> IO ()
putStr [] = return ()
putStr (x:xs) = do
    putChar x
    putStr xs

#****************************
print1 a = putStrLn . show $ a

main = print1(a)
    where
        a = [1, 2, 3]


# Solving problems
#**********************************
-- stud
-- Reverse polish notation
-- http://learnyouahaskell.com/functionally-solving-problems

solveRPN :: (Num a, Read a) => String -> a
solveRPN = head . foldl foldingFunction [] . words
    where   foldingFunction (x:y:ys) "*" = (x * y):ys
            foldingFunction (x:y:ys) "+" = (x + y):ys
            foldingFunction (x:y:ys) "-" = (y - x):ys
            foldingFunction xs numberString = read numberString:xs

#**********************************
-- stud
{-
http://learnyouahaskell.com/functionally-solving-problems
There are two main roads going from Heathrow to London and there's a number
of regional roads crossing them. It takes you a fixed amount of time to travel
from one crossroads to another. It's up to you to find the optimal path to take so
that you get to London as fast as you can! You start on the left side and can either
cross to the other main road or go forward.
-}

import Data.List

data Label = A | B | C deriving (Show)
type Path = [(Label, Int)]
data Section = Section { getA :: Int, getB :: Int, getC :: Int } deriving (Show)
--data Section = Section Int Int Int deriving (Show)
type RoadSystem = [Section]

roadStep :: (Path, Path) -> Section -> (Path, Path)
roadStep (pathA, pathB) (Section a b c) =
    let priceA = sum $ map snd pathA
        priceB = sum $ map snd pathB
        forwardPriceToA = priceA + a
        crossPriceToA = priceB + b + c
        forwardPriceToB = priceB + b
        crossPriceToB = priceA + a + c
        newPathToA = if forwardPriceToA <= crossPriceToA
                        then (A,a):pathA
                        else (C,c):(B,b):pathB
        newPathToB = if forwardPriceToB <= crossPriceToB
                        then (B,b):pathB
                        else (C,c):(A,a):pathA
    in  (newPathToA, newPathToB)

optimalPath :: RoadSystem -> Path
optimalPath roadSystem =
    let (bestAPath, bestBPath) = foldl roadStep ([],[]) roadSystem
    in  if sum (map snd bestAPath) <= sum (map snd bestBPath)
            then reverse bestAPath
            else reverse bestBPath

groupsOf :: Int -> [a] -> [[a]]
groupsOf 0 _ = undefined
groupsOf _ [] = []
groupsOf n xs = take n xs : groupsOf n (drop n xs)

main = do
    contents <- getContents
    let threes = groupsOf 3 (map read $ lines contents)
        roadSystem = map (\[a,b,c] -> Section a b c) threes
        path = optimalPath roadSystem
        pathString = concat $ map (show . fst) path
        pathPrice = sum $ map snd path
    putStrLn $ "The best path to take is: " ++ pathString
    putStrLn $ "The price is: " ++ show pathPrice

--cat paths.txt | runhaskell try.hs
{-
50
10
30
5
90
20
40
2
25
10
8
0
-}


{-
The best path to take is: BCACBBC
The price is: 75
-}


#************************************
-- Functor laws
{-
The first functor law states that if we map the id function over a functor,
the functor that we get back should be the same as the original functor.
If we write that a bit more formally, it means that fmap id = id

ghci> fmap id (Just 3)
Just 3
ghci> id (Just 3)
Just 3
ghci> fmap id [1..5]
[1,2,3,4,5]
ghci> id [1..5]
[1,2,3,4,5]
ghci> fmap id []
[]
ghci> fmap id Nothing
Nothing
-}

#***

{-
The second law says that composing two functions and then mapping the
resulting function over a functor should be the same as first mapping one
function over the functor and then mapping the other one. Formally written,
that means that fmap (f . g) = fmap f . fmap g. Or to write it in another way,
for any functor F, the following should hold: fmap (f . g) F = fmap f (fmap g F).
-}

#***

{-
If you want, we can check out how the second functor law holds for Maybe.
If we do fmap (f . g) over Nothing, we get Nothing, because doing a fmap with any
function over Nothing returns Nothing. If we do fmap f (fmap g Nothing), we get
Nothing, for the same reason. OK, seeing how the second law holds for Maybe if it's
a Nothing value is pretty easy, almost trivial.

How about if it's a Just something value? Well, if we do fmap (f . g) (Just x),
we see from the implementation that it's implemented as Just ((f . g) x), which is,
of course, Just (f (g x)). If we do fmap f (fmap g (Just x)), we see from the
implementation that fmap g (Just x) is Just (g x). Ergo, fmap f (fmap g (Just x))
equals fmap f (Just (g x)) and from the implementation we see that this equals
Just (f (g x)).

-}

#***

{-
Let's take a look at a pathological example of a type constructor being an instance of the Functor typeclass but not really being a functor, because it doesn't satisfy the laws. Let's say that we have a type:
-}

data CMaybe a = CNothing | CJust Int a deriving (Show)

instance Functor CMaybe where
    fmap f CNothing = CNothing
    fmap f (CJust counter x) = CJust (counter+1) (f x)

ghci> fmap id (CJust 0 "haha")
CJust 1 "haha"
ghci> id (CJust 0 "haha")
CJust 0 "haha"


#*********************************
-- stud
-- Various Functor instances

instance Functor ((->) r) where
    fmap f g = (\x -> f (g x))

instance Functor ((->) r) where
    fmap = (.)

instance Functor ((->) e) where
    fmap f g = g . f

instance Functor Maybe where
    fmap f (Just x) = Just (f x)
    fmap f Nothing = Nothing

instance Functor [] where
    fmap = map

instance Functor IO where
    fmap f action = do
        result <- action
        return (f result)

instance Functor (Either e) where
	fmap _ (Left a) = Left a
	fmap f (Right a) = Right (f a)

instance Functor ((,) e) where
    fmap f (c, v) = (c, f v)

#**********************************
-- Random

threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen =
    let (firstCoin, newGen) = random gen
        (secondCoin, newGen') = random newGen
        (thirdCoin, newGen'') = random newGen'
    in  (firstCoin, secondCoin, thirdCoin)


#*************************

randoms' :: (RandomGen g, Random a) => g -> [a]
randoms' gen = let (value, newGen) = random gen in value:randoms' newGen


#*****************************
-- stud

--Here's a little program that will make the user guess which number it's thinking of.

import System.Random
import Control.Monad(when)

main = do
    gen <- getStdGen
    askForNumber gen

askForNumber :: StdGen -> IO ()
askForNumber gen = do
    let (randNumber, newGen) = randomR (1,10) gen :: (Int, StdGen)
    putStr "Which number in the range from 1 to 10 am I thinking of? "
    numberString <- getLine
    when (not $ null numberString) $ do
        let number = read numberString
        if randNumber == number
            then putStrLn "You are correct!"
            else putStrLn $ "Sorry, it was " ++ show randNumber
        askForNumber newGen