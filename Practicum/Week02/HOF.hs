{-# LANGUAGE RankNTypes #-}
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
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# HLINT ignore "Collapse lambdas" #-}
{-# HLINT ignore "Eta reduce" #-}

module HOF where

import Prelude hiding (const, curry, id, log, map, on, swap, uncurry, until, ($), (.))

-- WARNING:
-- remind about github classrooms -> in-class && HW00
-- say soon HW01?
--
-- NOTE: talk about
-- @-bindings
-- guards
-- let
-- where
-- ?
-- lambdas, desugar function def, HOF(arguments), tuple-map hof example, currying, polymorphism
-- sections (why (+) makes sense)
-- logging as example for DI via higher order functions
-- ($), (.)
-- what does "combinator" mean?
--
-- TODO: implement
-- applyTwice :: (a -> a) -> a -> a
-- id
-- ($) - maybe go back to solutions and start rewriting stuff using ($)?
-- `infixr 0 $` (draw AST of an operator-heavy expr?)
-- (.)

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

id :: a -> a
id a = a

-- >>> applyTwice (\x -> x * 2) 5
-- 20

($) :: (a -> b) -> a -> b
-- ($) f x = f x
f $ x = f x

infixr 0 $

-- s (f (g (h x)))

-- FFFFF  $ XXXXXX

-- banica $ s $ f $ g $ h x

---

data Neshto = A Int | B Int Int

fff :: a -> (b -> (c -> d))
fff a b c = undefined

-- f(x) = x + 5

-- f :: Integer -> Integer -> Integer
-- f x y = x + y
-- f'' x = \y -> x + 5
-- f' = \x -> \y -> x + 5
-- f''' = \x y -> x + 5

-- f :: Neshto -> Int
-- f n@(A x) = undefined
-- f (B x y) = undefined

-- f :: Integer -> Integer
-- f x =
--   let z = let y :: Integer
--               y = 2 * x
--            in y + y
--    in z * 3
--
-- ff :: Integer
-- ff = x
--   where
--     x = x
--
-- g :: Integer -> Integer
-- g x =
--   let z = 2 * y
--    in z
--   where
--     y = 2 * x

-- >>> f 5
-- 20

-- TODO: live
data Tuple a b = MkTuple a b
  deriving (Show)

sumTuple :: Tuple Int Int -> Int
sumTuple (MkTuple x y) = x + y

-- template <typename A, typename B>
-- A fstTuple(Tuple<A,B>);
--
-- A fstTuple<A,B>(Tuple<A,B>)

-- TODO: implement, used in examples
fstTuple :: Tuple a b -> a
fstTuple (MkTuple x y) = x

sndTuple :: Tuple a b -> b
sndTuple (MkTuple x y) = y

-- sndTuple' :: Tuple a a -> a
-- sndTuple' (MkTuple x y) = x

-- EXERCISE
-- Take two arguments and return the first.
-- This is called const because if we think of it as a function
-- on one argument x, it returns a function that when called, always returns x
-- It is also practically always used partially applied.
-- EXAMPLES
-- >>> const 5 6
-- 5
-- >>> applyTwice (const 42) 1337
-- 42

const :: a -> b -> a
const x _ = x

-- EXERCISE
-- Compose two functions, very useful very often
-- there's a builtin (.) for this - the dot mimics mathematical notation f o g
-- EXAMPLES
-- >>> let f = compose (+3) (*5) in f 4
-- 23
-- >>> let f = compose (*5) (+5) in f 4
-- 45

compose :: (b -> c) -> (a -> b) -> a -> c
compose f g x = f $ g $ x

-- Play around with the syntax
(.) :: (b -> c) -> (a -> b) -> a -> c
(.) = compose

-- EXERCISE
-- Iterate a function f n times over a base value x.
-- EXAMPLES
-- >>> iterateN (+1) 1 10
-- 11
-- >>> iterateN (*2) 1 10
-- 1024

iterateN :: (a -> a) -> a -> Integer -> a
iterateN _ x 0 = x
iterateN f x n = iterateN f (f x) (n - 1)

-- EXERCISE
-- Swap the two elements of a tuple
-- EXAMPLES
-- >>> swap $ MkTuple 42 69
-- MkTuple 69 42

swap :: Tuple a b -> Tuple b a
swap (MkTuple a b) = (MkTuple b a)

-- EXERCISE
-- Apply a function to only the first component of a tuple
-- EXAMPLES
-- >>> first (*2) $ MkTuple 21 1337
-- MkTuple 42 1337

first :: (a -> b) -> Tuple a c -> Tuple b c
first f (MkTuple a c) = (MkTuple (f a) c)

-- EXERCISE
-- Convert a function operating on a tuple, to one that takes two arguments.
-- Called Curry after Haskell Curry - inventor of lambda calculus.
-- (Guess where the language gets its name from, ексди)
-- EXAMPLES
-- >>> curry (\(MkTuple x y) -> x * y) 23 3
-- 69

curry :: (Tuple a b -> c) -> a -> b -> c
curry f x y = f (MkTuple x y)

-- EXERCISE
-- Convert a two argument function, to one that takes a Tuple.
-- EXAMPLES
-- >>> uncurry (\x y -> x + y) $ MkTuple 23 46
-- 69

uncurry :: (a -> b -> c) -> Tuple a b -> c
uncurry f (MkTuple a b) = f a b

-- EXERCISE
-- > p `on` f
-- Implement a combinator that allows you to "preapply" a function f on the arguments of a function p
-- EXAMPLES
-- >>> let maxOnFirst = max `on` fstTuple in maxOnFirst (MkTuple 1 20) (MkTuple 2 100000)
-- 2

-- >>> let maxOnSum = max `on` sumTuple in maxOnSum (MkTuple 20 39) (MkTuple 12 34)
-- 59

on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
f `on` g = (\x y -> f (g x) (g y))

-- EXERCISE
-- Execute a function, until the result starts sastifying a given predicate
-- EXAMPLES
-- >>> until (>1000) (*7) 4
-- 1372

until :: (a -> Bool) -> (a -> a) -> a -> a
until p f x
    | (p x) = x
    | otherwise = until p f (f x)

-- EXERCISE
-- Apply two different functions to the two different arguments of a tuple
-- Think about what the type should be.
-- mapTuple :: ???
-- mapTuple = undefined

data Nat
  = Zero
  | Succ Nat
  deriving (Show)

-- EXERCISE
-- Look at addNat and multNat from last time.
--
-- addNat :: Nat -> Nat -> Nat
-- addNat Zero m = m
-- addNat (Succ n) m = Succ (addNat n m)
--
-- multNat :: Nat -> Nat -> Nat
-- multNat Zero _ = Zero
-- multNat (Succ n) m = addNat m (multNat n m)
--
-- They look very similar.
-- Can you implement a general enough higher-order function (called foldNat here), such that you can then use to
-- implement both of addNat and multNat by passing suitable arguments? What are those arguments?
--
-- foldNat :: ???
-- foldNat = ???
--
-- If your function is "good enough" you should also be able to implement exponentiation using it.
-- expNat :: Nat -> Nat -> Nat
-- expNat = undefined
--
-- Can you also implement
-- natToInteger :: Nat -> Integer
-- natToInteger = undefined
-- using your function? If not, modify your function so you can.
--
-- Can you also implement the following "predecessor" function using it? Yes/no, and why?
--
-- predNat :: Nat -> Nat
-- predNat Zero = Zero
-- predNat (Succ n) = n
--
-- If not, can you think of a foldNat' :: ??? with a different type signature, which would allow you to implement predNat?