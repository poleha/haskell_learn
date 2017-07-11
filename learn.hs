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