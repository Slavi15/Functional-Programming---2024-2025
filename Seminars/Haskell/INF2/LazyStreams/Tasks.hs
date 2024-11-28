{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# OPTIONS_GHC -fwarn-unused-matches #-}
{-# OPTIONS_GHC -fwarn-missing-signatures #-}
{-# OPTIONS_GHC -fwarn-name-shadowing #-}
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Werror #-}

nats :: [Integer]
nats = 0 : map (1+) nats

primes :: [Integer]
primes = sieve [2 ..]
    where
        sieve :: [Integer] -> [Integer]
        sieve [] = []
        sieve (x : xs) = x : sieve (filter (\y -> y `mod` x /= 0) xs)

iterate' :: (a -> a) -> a -> [a]
iterate' f x = x : iterate' f (f x)

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

--------------------------------------------

data Tree a = 
    Nil | 
    MkTree (Tree a) a (Tree a)
    deriving (Eq)

instance (Show a) => Show (Tree a) where
    show Nil = "Nil"
    show (MkTree lhs x rhs) = "Node " ++ show x ++ " (" ++ show lhs ++ ") (" ++ show rhs ++ ")"

foldTree :: (b -> a -> b -> b) -> b -> Tree a -> b
foldTree _ acc Nil = acc
foldTree f acc (MkTree lhs x rhs) = f (foldTree f acc lhs) x (foldTree f acc rhs)

trimTree :: Integer -> Tree a -> Tree a
trimTree _ Nil = Nil
trimTree 0 _ = Nil
trimTree depth (MkTree lhs x rhs) = MkTree (trimTree (depth - 1) lhs) x (trimTree (depth - 1) rhs)

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f = foldTree (\lhs x rhs -> (MkTree lhs (f x) rhs)) Nil

-- babaTree :: Tree String
-- babaTree = buildTree ""
--     where 
--         buildTree :: String -> Tree String
--         buildTree str = MkTree (buildTree (str ++ "a")) str (buildTree (str ++ "b"))