data Writ w a = Writ {getWrit :: (a, w)} deriving (Show)


instance Functor (Writ w) where
    fmap f (Writ (a, w)) = Writ ((f a), w)


instance Monoid w => Applicative (Writ w) where
    pure x = Writ (x, mempty)
    Writ (f, w1) <*> Writ (x, w2) = Writ ((f x), w1 `mappend` w2)

instance Monoid w => Monad (Writ w) where
    return x = Writ (x, mempty)
    Writ (x, w) >>= f = let Writ (y, w1) = f x in Writ (y, w `mappend` w1)
    a >> b = a >>= \_ -> b


tell::Monoid w => w -> Writ w ()
tell w = Writ ((), w)

{-

main = do
    let writVal = Writ (1, ["init"])
        mappedWritVal = fmap (\a -> a + 1) writVal
        writFunc = Writ ((\x -> x * 2), ["*2"])
        res = writFunc <*> mappedWritVal
    print(res)
--Writ {getWrit = (4,["*2","init"])}

-}


{-
main = do
    let writVal = Writ (1, ["init"])
        writFunc x = Writ (x * 2, ["*2"])
        res = writVal >>= writFunc >> Writ (0, ["Replacing value but keeping log"])
    print(res)

-}


main = do
    let writVal = Writ (1, ["init"])
        writFunc:: (Num a) => a -> Writ [String] a
        writFunc x = Writ (x * 2, ["*2"])
        res = do
            x <- writVal
            tell ["Tell here"]
            y <- writFunc x
            Writ (x + y, ["Replacing"])
        --res = writVal >> tell ["Tell here"] >>= writFunc
    print(res)


