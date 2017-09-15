data Writ a = Writ {getData :: a, getLog :: String} deriving (Show)


instance Functor Writ where
    fmap f (Writ x log_x) = Writ (f x) log_x

instance Applicative Writ where
    pure x = Writ x ""
    Writ f log_f <*> Writ x log_x = Writ (f x) (log_x ++ log_f)

instance Monad Writ where
    return x = Writ x ""
    Writ x log_x >>= f = res
        where Writ a log_a = f x
              res = Writ a (log_x ++ ", " ++ log_a)


add_one x = Writ (x + 1) "Added one"

main = do
    let w = Writ 5 "Initial message"
        res = w >>= add_one
    print(res)