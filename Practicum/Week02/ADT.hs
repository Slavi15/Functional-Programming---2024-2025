{-# LANGUAGE EmptyDataDeriving #-}
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

module ADTs where

-- TODO: talk about
-- * pls join+write on discord
-- * solutions (tagged on discord)
-- * first homework this week
-- show:
-- * pragmas on the top of files
-- * where from last week
-- * guards
-- * remind about sections

----------------------------------------------
-- RPS (Rock, Paper, Scissors) and Booleans --
----------------------------------------------

-- RPS - enum example
-- This will be our type representing Rock, Paper, and Scissors
-- show case here
data RPS = Rock | Paper | Scissors
  deriving (Show)

-- beats function: Implement the logic for which RPS beats another
-- Use pattern matching and ignore some cases to demonstrate the use of _
-- EXAMPLES
-- >>> beats Rock Paper
-- False
-- >>> beats Paper Rock
-- True
beats :: RPS -> RPS -> Bool
beats Rock Scissors = True
beats Scissors Paper = True
beats Paper Rock = True
beats _ _ = False

-- TASK
-- Define the "next" throw you can do in the "usual" ordering of RPS
-- i.e. @next x@ should be the throw that beats x
-- EXAMPLES
-- >>> next Rock
-- Paper
next :: RPS -> RPS
next Rock = Paper
next Paper = Scissors
next Scissors = Rock
-- sugar syntax for the code below

next' :: RPS -> RPS
next' x = case x of
    Rock -> Paper
    Paper -> Scissors
    Scissors -> Rock

-- TASK
-- Define what it means for two RPS values to be equal
-- Use pattern matching and use _ matches!
-- EXAMPLES
-- >>> eqRPS Rock Rock
-- True
-- >>> eqRPS Rock Paper
-- False
eqRPS :: RPS -> RPS -> Bool
eqRPS Rock Rock = True
eqRPS Paper Paper = True
eqRPS Scissors Scissors = True
eqRPS _ _ = False

-- TASK
-- Define a shorter version of beats using next and eqRPS
-- EXAMPLES
-- >>> beats' Rock Paper
-- True
-- >>> beats' Paper Scissors
-- True
beats' :: RPS -> RPS -> Bool
beats' x y = next x `eqRPS` y

------------
-- Points --
------------

-- Record syntax for a point in 2D discrete space
data Point = MkPoint Integer Integer
  deriving (Show)

-- |
-- Check if a point is in the first quadrant (both x and y are positive)
isInFirstQuadrant :: Point -> Bool
isInFirstQuadrant (MkPoint x y) = x > 0 && y > 0

inWhichQuadrantIsIn :: Point -> Integer
inWhichQuadrantIsIn (MkPoint x y) = if x > 0 && y > 0 then 1 else undefined

inWhichQuadrantIsIn' :: Point -> Integer
inWhichQuadrantIsIn' (MkPoint x y)
  | x >= 0 && y >= 0 = 1
  | x  < 0 && y >= 0 = 2
  | x  < 0 && y  < 0 = 3
  | x >= 0 && y  < 0 = 4
  | otherwise = error "the universe has stopped working"

-- |
-- Invert a point by swapping the signs of x and y
invert :: Point -> Point
invert (MkPoint x y) = MkPoint (-x) (-y)

----------------------------------------
-- Natural Numbers (Peano Arithmetic) --
----------------------------------------

-- Encoding for natural numbers using Peano arithmetic
data Nat = Zero | Succ Nat
  deriving (Show)

-- 2
-- Succ (Succ Zero)
-- 1+ 1+ 0

-- TASK
-- Convert an Integer to Nat
-- EXAMPLES
-- >>> integerToNat 3
-- Succ (Succ (Succ Zero))
integerToNat :: Integer -> Nat
integerToNat x =
  if x == 0
  then Zero
  else Succ (integerToNat (x - 1))

-- TASK
-- Convert a Nat back to an Integer
-- EXAMPLES
-- >>> natToInteger (Succ (Succ Zero))
-- 2
natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ x) = 1 + natToInteger x

-- TASK
-- Add two Nats
-- EXAMPLES
-- >>> addNat (Succ Zero) (Succ Zero)
-- Succ (Succ Zero)
addNat :: Nat -> Nat -> Nat
addNat Zero n2 = n2
addNat (Succ n1) n2 = Succ (addNat n1 n2)

-- addNat (Succ (Succ Zero))    (Succ Zero)
--              ^^^^^^^^^^^ n1  ^^^^^^^^^^^ n2 
-- Succ (addNat (Succ Zero) (Succ Zero))
--                    ^^^ n1 ^^^^^^^^^ n2
-- Succ (Succ (addNat Zero (Succ Zero)))
-- Succ (Succ (Succ Zero))

-- TASK
-- Multiply two Nats
-- EXAMPLES
-- >>> multNat (Succ (Succ Zero)) (Succ (Succ (Succ Zero)))
-- Succ (Succ (Succ (Succ (Succ (Succ Zero)))))
multNat :: Nat -> Nat -> Nat
multNat Zero n2 = n2
multNat (Succ n1) n2 = n2 `addNat` (n1 `multNat` n2)
-- multNat (Succ n1) n2 = addNat n2 (multNat n1 n2)

-- TASK
-- Compare two Nats, returning an Ordering
-- EXAMPLES
-- >>> compareNat (Succ Zero) Zero
-- GT
compareNat :: Nat -> Nat -> Ordering
compareNat Zero Zero = EQ
compareNat (Succ _) Zero = GT
compareNat Zero (Succ _) = LT
-- compareNat (Succ n1) (Succ n2) = compareNat n1 n2
compareNat (Succ n1) (Succ n2) =  n1 `compareNat` n2

-- TASK
-- Return the maximum of two Nats
-- EXAMPLES
-- >>> maxNat (Succ Zero) (Succ (Succ Zero))
-- Succ (Succ Zero)
maxNat :: Nat -> Nat -> Nat
maxNat Zero n2 = n2
maxNat n1 Zero = n1
maxNat (Succ n1) (Succ n2) = Succ (n1 `maxNat` n2)

-----------------
-- Expressions --
-----------------

-- A simple expression language for a calculator
data Expr
  = Val Integer
  | Plus Expr Expr
  | Mult Expr Expr
  deriving (Show)

infixr 7 `Plus`
infixr 8 `Mult`

-- TASK
-- Evaluate an expression in the Expr language
-- EXAMPLES
-- >>> eval (Val 3)
-- 3
-- >>> eval (Plus (Val 3) (Val 4))
-- 7
-- >>> eval (Mult (Val 3) (Val 4))
-- 12
eval :: Expr -> Integer
eval (Val x) = x
eval (Plus x y) = eval x + eval y
eval (Mult x y) = eval x * eval y

-- TASK
-- Extend the Expr language with If expressions
-- Interpret 0 as "false" and any non-zero value as "true"

------------
-- Belote --
------------

-- Data type for Ranks in a card game
data Rank = Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
  deriving (Show)

-- Data type for Suits in a card game
data Suit = Clubs | Diamonds | Hearts | Spades
  deriving (Show)

-- TASK
-- Check if two suits are equal
suitEquals :: Suit -> Suit -> Bool
suitEquals Clubs Clubs = True
suitEquals Diamonds Diamonds = True
suitEquals Hearts Hearts = True
suitEquals Spades Spades = True
suitEquals _ _ = False

-- Record syntax for representing a Card
data Card = MkCard Rank Suit
  deriving (Show)

-- Data type for Contracts in the Belote game
data Contract = MkTrumps Suit | NoTrump | AllTrump
  deriving (Show)

-- TASK
-- Check if a card is of a trump suit based on a given contract
isTrump :: Contract -> Card -> Bool
isTrump (MkTrumps trumpSuit) (MkCard _rank suit) = trumpSuit `suitEquals` suit
isTrump AllTrump _ = True
isTrump NoTrump _ = False

-- TASK
-- Assign a numerical power value to a card based on the contract
-- Ensure that higher power values represent stronger cards
cardPower :: Contract -> Card -> Integer
cardPower contract card@(MkCard rank _suit) =
  if isTrump contract card
    then rankPowerTrump
    else rankPowerNoTrump
  where
    rankPowerTrump :: Integer
    rankPowerTrump = case rank of
      Seven -> 0
      Eight -> 1
      Queen -> 2
      King -> 3
      Ten -> 4
      Ace -> 5
      Nine -> 6
      Jack -> 7

    rankPowerNoTrump :: Integer
    rankPowerNoTrump = case rank of
      Seven -> 0
      Eight -> 1
      Nine -> 2
      Jack -> 3
      Queen -> 4
      King -> 5
      Ten -> 6
      Ace -> 7

-- @ allows you to keep a reference to the whole value (in this case, the card) while also breaking it down into its components.

-- TASK
-- | A data type to describe the different ways two cards can relate, given a contract
-- See the 'sameSuit' and 'relateCards' functions below to get a better sense of how
-- you'll be producing this data type, and hence what constructors it should have
--
-- This data type exists mainly because it's useful as a tool to implement the 'fight' function
-- As such, it might be the case that your version of CardRelation is different from what I intended
--
-- The way to think about this is as follows:
-- Imagine you're in a situation where someone has played a card, and you've just played a card
-- You now need to decide which card would beat the other one
-- 'CardRelation' expresses the first thing you need to calculate in regards to the two cards
-- *before* you can start checking their 'cardPower's, e.g. is one of them a trump and so on
--
-- HINT:
-- The intended solution has 4 constructors
data CardRelation = FirstIsTrump | SecondIsTrump | SameSuit | DifferentSuit
  deriving (Show)

-- TASK
-- Given a contract, calculate how two cards relate
relateCards :: Contract -> Card -> Card -> CardRelation
relateCards contract card1@(MkCard _ card1Suit) card2@(MkCard _ card2Suit) =
  case (isTrump contract card1, isTrump contract card2) of
    (False, False) ->
      if card1Suit `suitEquals` card2Suit
        then SameSuit
        else DifferentSuit
    (False, True) -> SecondIsTrump
    (True, False) -> FirstIsTrump
    (True, True) -> SameSuit

-- TASK
-- Given a contract and two cards, return the winning card
-- Assume the first card is played first
fight :: Contract -> Card -> Card -> Card
fight contract card1 card2 =
  case relateCards contract card1 card2 of
    SameSuit ->
      if cardPower contract card1 <= cardPower contract card2
        then card1
        else card2
    DifferentSuit -> card1
    FirstIsTrump -> card1
    SecondIsTrump -> card2


-- Data type for a trick (игра, разигравка, ръка, рунд), consisting of four cards
data Trick = MkTrick Card Card Card Card
  deriving (Show)

-- TASK
-- Given a contract and a Trick, determine the winning card
-- Remember that the leftmost card was played first
winner :: Contract -> Trick -> Card
winner contract (MkTrick card1 card2 card3 card4) =
  ((card1 `fight'` card2) `fight'` card3) `fight'` card4
  where
    fight' card = fight contract card

-- TASK
-- Check if a Trick could have been played according to the rules of Belote
isValid :: Contract -> Trick -> Bool
isValid = undefined