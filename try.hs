main = do
    let h = (2+) >>= \x -> (3*) >>= \y -> return (x,y)
        res = h 3
    print(res)



main = do
    let h = do
            x <- (2+)
            y <- (3*)
            return (x, y)
        res = h 3
    print(res)




