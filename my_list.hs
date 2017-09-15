data MyList a = Empty | Cons a (MyList a) deriving (Show)

instance Functor MyList where
    fmap f Empty = Empty
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)


instance Applicative MyList where
    pure x = Cons x Empty
    Empty <*> _ = Empty
    --Cons f fs <*> Cons x xs = f x `Cons` (fs <*> xs)



--instance Monad MyList where
--    return x = Cons x Empty
--    Empty >>= _ = Empty
--    Cons x >>= f = fmap f x


main = do
    --let list = 1 `Cons` (2 `Cons` Empty)
    --    res = fmap (\x -> x + 1) list
    --print(res)

    let fs = (+1) `Cons` ((+2) `Cons` Empty)
        xs = 1 `Cons` (2 `Cons` Empty)
        res = fs <*> xs
    print(res)