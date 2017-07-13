main = print(filter' (>1) [1,2,3])

filter' _ [] = []
filter' f x = [e | e <- x, f e == True]

