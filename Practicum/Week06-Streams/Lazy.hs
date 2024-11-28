-- cover all cases!
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
-- warn about incomplete patterns v2
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}
-- write all your toplevel signatures!
{-# OPTIONS_GHC -fwarn-missing-signatures #-}
-- use different names!
{-# OPTIONS_GHC -fwarn-name-shadowing #-}
-- use all your pattern matches!
{-# OPTIONS_GHC -fwarn-unused-matches #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Redundant lambda" #-}

module Lazy where

import Prelude hiding (cycle, foldl, foldr, repeat, scanl)

-- TODO:
-- (boolean blindness)
-- https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/
-- (a → Maybe b) VS (a → Bool, a ⇀/↛/↪ b) -- (funny symbol for partial function)
-- thunks -- delayed computation, replaced when done; memoization incarnate
-- :print, :sprint
-- (don't forget type annos; sometimes unreliable, different results between ghc versions)
-- (maybe try `:set -XMonomorphismRestriction` for some (polymorphic) examples)
-- when do we "need" to evalute something? case matches!
-- pattern-matching desuraging (Nat, isZero, tuples)
-- how much do we need to evaluate it?
-- WHNF (the least that we need to do to continue forward)
-- infinite structures (again)
-- Stream, but we will use [] with asserts*
-- error "banica"
-- show take def?
-- Debug.Trace

-- banica :: [Integer] -> Bool
-- -- banica (x : xs) = True
-- -- banica [] = False
-- banica = \xs -> case neshto of
--   (chislo, xs) -> True

-- fst' :: (a, b) -> a
-- fst' = \pair -> case pair of
--   (x, _) -> x

-- foldr, foldl, stack space
--
-- @seq :: a -> b -> b@
-- @seq a b@ - "evaluate a and b together" - no enforced order, but usually used to mean a before b
--  @seq a b@ terminates iff a terminates ("terminates" meaning that it evaluates to something)
--
-- very detailed SO answer describing seq in more detail:
-- https://stackoverflow.com/a/66965677
--
-- Quoting: <<EOQ
-- A note on evaluation order: the expression @seq a b@ does not guarantee that @a@ will be evaluated before @b@.
-- The only guarantee given by @seq@ is that the both @a@ and @b@ will be evaluated before seq returns a value.
-- In particular, this means that @b@ *may* be evaluated before @a@.
-- It also means that if you entirely ignore the result of @seq a b@, neither @a@ nor @b@ will be evaluated,
-- for example in @const 5 (seq undefined undefined)@.
-- EOQ
--
-- TODO:
-- funny unicode symbol (like `⋅`) for `f`
-- foldl' with seq and bang (syntax sugar)
-- draw evaluation "sequence"
-- mention deepseq

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl _ acc [] = acc
foldl f acc (x : xs) = foldl f (f acc x) xs

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ acc [] = acc
foldl' f !acc (x : xs) = foldl' f (f acc x) xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ nv [] = nv
foldr f nv (x : xs) = x `f` foldr f nv xs

length' :: [a] -> Integer
length' = foldl (\acc _ -> acc + 1) 0

-- >>> length' [1,2,3]
-- 3

-- length (1 : 2 : 3 : [])
-- foldl' (\acc _ -> acc + 1) 0 (1 : 2 : 3 : [])
-- foldl' (\acc _ -> acc + 1) 0 (1 : 2 : 3 : [])
-- foldl' _ ((\acc _ -> acc + 1) 0 1) (2 : 3 : [])
-- foldl' _ (0 + 1) (2 : 3 : [])
-- foldl' _ (1 + 1) (3 : [])
-- foldl' _ (2 + 1) ([])
-- 3

reverseAndMap' :: (a -> b) -> [a] -> [b]
reverseAndMap' f = foldr (\x ys -> f x : ys) []

-- map' (1+) [1,2,3]
-- foldl (\ys x -> f x : ys) [] [1,2,3]
-- foldl (\ys x -> f x : ys) ((1+) 1 : []) [2,3]
-- foldl (\ys x -> f x : ys) ((1+) 2 : ((1+) 1 : [])) [3]
-- foldl (\ys x -> f x : ys) ((1+) 3 : ((1+) 2 : ((1+) 1 : []))) []
-- ((1+) 3 : ((1+) 2 : ((1+) 1 : [])))

-- foldr (:) [] [1,2,3]
-- foldr [] (\ys x -> f x : ys) [1,2,3]
-- 1 : foldr [] (\ys x -> f x : ys)  [2,3]
-- 1 : 2 : foldr [] (\ys x -> f x : ys) ((1+) 2 : ((1+) 1 : [])) [3]
-- 1 : 2 : 3 : foldr [] (\ys x -> f x : ys) ((1+) 3 : ((1+) 2 : ((1+) 1 : []))) []
-- 1 : 2 : 3 : []


-- TODO:
-- seq the acc
-- bang the acc

-- EXERCISE
-- Infinitely repeat a value
-- >>> take 4 $ repeat 'a'
-- "aaaa"

repeat :: a -> [a]
repeat x = x : repeat x

-- EXERCISE
-- A list of all the natural numbers.
-- EXAMPLES
-- >>> take 10 nats
-- [0,1,2,3,4,5,6,7,8,9]

nats :: [Integer]
-- nats = [0 ..]
nats = 0 : map (1+) nats

-- EXERCISE
-- Generate an infinite list of numbers, starting with the given number, with the given interval between each numbe.
-- EXAMPLES
-- >>> take 10 $ fromThen 0 1
-- [0,1,2,3,4,5,6,7,8,9]
-- >>> take 20 $ fromThen 0 1
-- [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19]
-- >>> take 10 $ fromThen 4 9
-- [4,13,22,31,40,49,58,67,76,85]
-- >>> take 10 $ fromThen 0 (-10)
-- [0,-10,-20,-30,-40,-50,-60,-70,-80,-90]

fromThen :: Integer -> Integer -> [Integer]
fromThen a b = a : fromThen (a + b) b

-- EXERCISE
-- Implement a list of all the factorial numbers
-- Use a where or let to "cache" the current number, so we don't do all the multiplications every time.
-- i.e. if we've already calculated 5!, we can simply multiply the result by 6, we don't need to calculate 5! again.
-- EXAMPLES
-- >>> take 10 facts
-- [1,1,2,6,24,120,720,5040,40320,362880]

facts :: [Integer]
facts = facts' 1 1
    where
        facts' :: Integer -> Integer -> [Integer]
        facts' n prev = (n * prev) : facts' (n + 1) (n * prev)

-- EXERCISE
-- "Caching foldl"
-- It's sometimes useful to have all the "intermediate" results of a fold. It's also helpful for debugging sometimes.
-- Implement a version of foldl that returns all of the intermediate results it has.
-- These are called "scans" in the Haskell standard library.
-- EXAMPLES
-- >>> scanl (+) 0 [1..10]
-- [0,1,3,6,10,15,21,28,36,45,55]

scanl :: (b -> a -> b) -> b -> [a] -> [b]
scanl _ acc [] = [acc]
scanl f acc (x : xs) = acc : scanl f (f acc x) xs

-- EXERCISE
-- Use scanl to implement facts.
-- EXAMPLES
-- >>> take 10 factsScanl
-- [1,1,2,6,24,120,720,5040,40320,362880,3628800]

factsScanl :: [Integer]
factsScanl = scanl (\x acc -> x * acc) 1 [1 ..]

-- EXERCISE
-- Implement a list of all the fibonacci numbers.
-- Use the following idea:
-- The fibonacci numbers start with 0 1
-- To generate the next fibonacci number, we need to sum the previous two, so assuming we already have
-- fibs :: [Integer]
-- that would mean summing the head of fibs with the head of the tail of fibs
-- zipWith will be useful here.
-- EXAMPLES
-- >>> take 10 fibs
-- [0,1,1,2,3,5,8,13,21,34]

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- EXERCISE
-- Idea:
-- We can get the all the primes with the following algorithm.
-- Let's start with all the numbers >=2
-- The current number - call it x - is prime (this is true in the beginning - x = 2 is prime) - leave it in our list
-- All of the other numbers that are divisible by x aren't prime - filter them out
-- This is called the sieve of Eratosthenes
-- Implement this for a given input list
-- Now all we need to do is apply it to all the natural numbers >=2 to get a list of all the primes.
-- EXAMPLES
-- >>> take 10 $ primes
-- [2,3,5,7,11,13,17,19,23,29]
-- >>> take 20 $ primes
-- [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71]

isqrt :: Integer -> Integer
isqrt = floor . sqrt . fromIntegral

isPrime :: Integer -> Bool
isPrime k
    | k > 1 = null [x | x <- [2 .. (isqrt k)], k `mod` x == 0]
    | otherwise = False

primes :: [Integer]
primes = eratosthenes [2 ..]
  where
    eratosthenes :: [Integer] -> [Integer]
    eratosthenes (x : xs) = x : eratosthenes (filter (\y -> y `mod` x /= 0) xs)

-- EXERCISE
-- Infinitely repeat a list
-- >>> take 7 $ cycle [1,2,3]
-- [1,2,3,1,2,3,1]

cycle :: [a] -> [a]
cycle xs = xs ++ cycle xs

-- Let's consider the following problem:
-- We have a "circle" of n people. We have an integer k.
-- We repeat the following procedure, starting from the first person, until there is only one left:
--
-- "Kill" the kth person. Repeat this, "starting" from the k+1th person, i.e. we start indexing from the k+1th person.
--
-- Example execution for n = 5, k = 2:
-- 1 2 3 4 5 (start)
-- 1 2 4 5 (we kill 3, since 0 + 2 = 2)
-- 2 4 5 (we kill 1, since 4 + 2 = 6, circling back to 1)
-- 2 4
-- 4
--
-- The question is, given arguments n and k, what's the number of the final person left. (4 in the example above)
--
-- This is called the josephus problem - see https://en.wikipedia.org/wiki/Josephus_problem.
--
-- Your task is to implement @jos :: Integer -> Int -> Integer@
-- which takes n and k, and returns the last surviving person.
--
-- Your hint is to use `cycle` to express our circle, and then to drop and filter things from it.
-- E.g. the representation of our circle for n = 5 would be [0,1,2,3,4,0,1,2,3,4,0,1,2,3,4...]
-- EXAMPLES
-- >>> jos 5 2
-- 4
-- >>> jos 10 4
-- 3
-- >>> jos 10 8
-- 7
-- >>> map (\(x,y) -> (x, y, jos x y)) [(x, y) | x <- [2..5], y <- [2..5]]
-- [(2,2,2),(2,3,1),(2,4,2),(2,5,1),(3,2,2),(3,3,2),(3,4,1),(3,5,1),(4,2,1),(4,3,2),(4,4,2),(4,5,3),(5,2,4),(5,3,1),(5,4,2),(5,5,4)]

jos :: Integer -> Int -> Integer
jos = undefined
  where
    -- figure out what this function should do based only on the types and the name
    -- I think there's only one valid type safe implementation of this
    -- ask me if you're confused
    -- this function exists in base but with Bool instead of Maybe
    untilJust :: (a -> Maybe b) -> (a -> a) -> a -> b
    untilJust = undefined
    -- the procedure which actually does the removal
    go :: [Integer] -> [Integer]
    go = undefined