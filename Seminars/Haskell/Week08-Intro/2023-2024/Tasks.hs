{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}     -- cover all cases!
{-# OPTIONS_GHC -fwarn-unused-matches #-}          -- use all your pattern matches!
{-# OPTIONS_GHC -fwarn-missing-signatures #-}      -- write all your toplevel signatures!
{-# OPTIONS_GHC -fwarn-name-shadowing #-}          -- use different names!
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-} -- warn about incomplete patterns v2
{-# OPTIONS_GHC -Werror #-}                        -- turn warnings into errors

fact :: Integer -> Integer
fact 0 = 1
fact n = n * fact (n - 1)

factorial :: Integer -> Integer
factorial n = factorial' n 1
    where
        factorial' :: Integer -> Integer -> Integer
        factorial' 0 res = res
        factorial' x res = factorial' (x - 1) (res * x)

fibb :: Integer -> Integer
fibb 0 = 1
fibb 1 = 1
fibb n = fibb (n - 1) + fibb (n - 2)

fibonacci :: Integer -> Integer
fibonacci n = fibonacci' 0 1 n
    where
        fibonacci':: Integer -> Integer -> Integer -> Integer
        fibonacci' _ y 0 = y
        fibonacci' x y nth = fibonacci' y (x + y) (nth - 1)

compose :: (b -> c) -> (a -> b) -> a -> c
compose f g x = f $ g $ x

myConcat :: [a] -> [a] -> [a]
myConcat [] ys = ys
myConcat (x : xs) ys = x : myConcat xs ys

myConcat' :: [a] -> [a] -> [a]
myConcat' xs ys = xs ++ ys

isPrefix :: (Eq a) => [a] -> [a] -> Bool
isPrefix _ [] = False
isPrefix [] _ = True
isPrefix (x : xs) (y : ys) = if x == y then (isPrefix xs ys) else False

isPrefix' :: (Eq a) => [a] -> [a] -> Bool
isPrefix' xs ys = (take (length xs) $ ys) == xs

frepeat :: Int -> (a -> a) -> a -> a
frepeat 0 _f x = x
frepeat n f x = frepeat (n - 1) f (f x)