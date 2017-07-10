main = print(replicate' 3 5)


replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' 0 x = []
replicate' 1 x = [x]
replicate' n x = x:replicate' (n-1) x




