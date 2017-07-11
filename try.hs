main = print(sum1 [1, 2, 3])

-- sum1 [] = 0
sum1 [x] = x
sum1 (x : xs) = x + sum1 xs


