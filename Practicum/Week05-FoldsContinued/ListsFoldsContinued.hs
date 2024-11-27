{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Use map" #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# HLINT ignore "Use foldl" #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
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

module ListsFoldsContinued where

import Prelude hiding (Either(..), reverse, zip, zipWith, null)

-- WARNING:
-- second homework

-- TODO:
-- tail recursion
-- reverseLinear :: [a] -> [a] -- (without and with foldl)
-- Either (Maybe<->null == Either<->Exceptions)
-- folds as destructors, mirrored to constructors, again (drive the point home)
-- (hard-ish exercises from last time, mostly mind-bending foldr usages)
-- null :: [a] -> Bool
-- lastMaybe :: [a] -> Maybe a

factorialSlow :: Integer -> Integer
factorialSlow 0 = 1
factorialSlow n = n * factorialSlow (n - 1)

factorial :: Integer -> Integer
factorial = go 1
  where
  go :: Integer -> Integer -> Integer
  go result 0 = result
  go result n = go (n * result) (n - 1)

-- >>> map factorial [1..10]
-- [1,2,6,24,120,720,5040,40320,362880,3628800]

reverseLinear :: [a] -> [a]
reverseLinear = go []
  where
    go :: [a] -> [a] -> [a]
    go result [] = result
    go result (x : xs) = go (x : result) xs

-- foldl :: (b -> a -> b) -> b -> [a] -> b
-- foldl rekursivno bazovo [] = bazovo
-- foldl rekursivno bazovo (x : xs) = foldl rekursivno (rekursivno bazovo x) xs

reverseLinear' :: [a] -> [a]
reverseLinear' = foldl (flip (:)) []

-- >>> reverseLinear [1..10]
-- [10,9,8,7,6,5,4,3,2,1]

data Either a b = Left a | Right b

-- Result<t, e> -> Ok(t), Err(e)

null :: [a] -> Bool
null = foldr (\_ _ -> False) True

-- >>> null [undefined]
-- False

lastMaybe :: [a] -> Maybe a
lastMaybe = foldr newLast Nothing
  where
    newLast x Nothing = Just x
    newLast _ (Just y) = Just y

-- >>> lastMaybe [1,2,3]
-- Just 3

safeDiv :: Integer -> Integer -> Maybe Integer
safeDiv _ 0 = Nothing
safeDiv n m = Just $ n `div` m

-- | The @DZero@ has a D because Nat also has a constructor called @Zero@
data Digit
  = DZero
  | One
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  deriving (Show, Enum)

-- EXERCISE
-- Parse a character into a digit.
-- The easiest way is to pattern match on all the cases.
--
-- OPTIONAL: You can try to use the LambdaCase extension here, in order to avoid writing parseDigit 10 times:
-- First, you need to add {-# LANGUAGE LambdaCase #-} to the top of the file (it's already enabled in this file)
-- Second, lambda case is used as follows:
-- Every time you write @\case ...@, that is desugared into @\x -> case x of ...@
-- For example:
-- safeDiv n = \case
--   0 -> Nothing
--   m -> ...
--
-- EXAMPLES
-- >>> parseDigit '6'
-- Just Six
-- >>> parseDigit '9'
-- Just Nine
-- >>> parseDigit 'c'
-- Nothing

parseDigit :: Char -> Maybe Digit
parseDigit '0' = Just DZero
parseDigit '1' = Just One
parseDigit '2' = Just Two
parseDigit '3' = Just Three
parseDigit '4' = Just Four
parseDigit '5' = Just Five
parseDigit '6' = Just Six
parseDigit '7' = Just Seven
parseDigit '8' = Just Eight
parseDigit '9' = Just Nine
parseDigit _ = Nothing

-- EXERCISE
-- See if all the values in a list xs are Just, returning Just xs only if they are.
-- We can think of this as all the computations in a list "succeeding",
-- and therefore the entire "computation list" has "succeeded.
-- Note that it is vacuously that all the elements in the empty list are Just.
-- EXAMPLES
-- >>> validateList []
-- Just []
-- >>> validateList [Just 42, Just 6, Just 9]
-- Just [42,6,9]
-- >>> validateList [Nothing, Just 6, Just 9]
-- Nothing
-- >>> validateList [Just 42, Nothing, Just 9]
-- Nothing
-- >>> validateList [Just 42, Just 6, Nothing]
-- Nothing

validateList :: [Maybe a] -> Maybe [a]
validateList xs = foldr step (Just []) xs
    where
        step (Just x) (Just acc) = Just (x : acc)
        step Nothing _ = Nothing
        step _ _ = Nothing

-- EXERCISE
-- You often have a collection (list) of things, for each of which you want to
-- perform some computation, that might fail (returning Maybe).
-- Let's implement a function to do exactly this -
-- execute a "failing computation" for all the items in a list,
-- immediately "aborting" upon a failure.
-- Think about how to reuse validateList.
-- This is called traverseListMaybe, because it's a special case of a generic function called traverse
-- that performs "actions" for each element of a "collection", specialised to List and Maybe
-- EXAMPLES
-- >>> traverseListMaybe (\x -> if even x then Just x else Nothing) [2,4,6]
-- Just [2,4,6]
-- >>> traverseListMaybe (\x -> if even x then Just x else Nothing) [1,2,3]
-- Nothing
-- >>> traverseListMaybe (5 `safeDiv`) [0,2]
-- Nothing
-- >>> traverseListMaybe (8 `safeDiv`) [3,2]
-- Just [2,4]

traverseListMaybe :: (a -> Maybe b) -> [a] -> Maybe [b]
traverseListMaybe f xs = foldr step (Just []) xs
    where
        step x (Just acc) = case (f x) of
            (Just y) -> Just (y : acc)
            Nothing -> Nothing
        step _ Nothing = Nothing

-- EXERCISE
-- Convert a list of digits to a number.
--
-- You can use @fromEnum :: Digit -> Int@ to convert digit into an @Int@
-- and @fromIntegral :: Int -> Integer@ to convert an @Int@ into an @Integer@
--
-- Assume that the empty list converts to 0.
-- HINT: It might be easier to first reverse the list and then operate on it with a helper.
-- EXAMPLES
-- >>> digitsToNumber [Six,Nine]
-- 69
-- >>> digitsToNumber [One,Two,DZero]
-- 120
-- >>> digitsToNumber [DZero,One,Two,DZero]
-- 120

digitToNumber :: Digit -> Integer
digitToNumber DZero = 0
digitToNumber One = 1
digitToNumber Two = 2
digitToNumber Three = 3
digitToNumber Four = 4
digitToNumber Five = 5
digitToNumber Six = 6
digitToNumber Seven = 7
digitToNumber Eight = 8
digitToNumber Nine = 9

myConcat :: (Integral a) => a -> a -> a
myConcat n digit = (n * 10) + digit

digitsToNumber :: [Digit] -> Integer
digitsToNumber xs = foldl (\acc x -> myConcat acc (digitToNumber x)) 0 xs

-- EXERCISE
-- Combine the previous functions to parse a number.
-- EXAMPLES
-- >>> parseNumber "0"
-- Just 0
-- >>> parseNumber "3"
-- Just 3
-- >>> parseNumber "69"
-- Just 69
-- >>> parseNumber "0123"
-- Just 123
-- >>> parseNumber "blabla"
-- Nothing
-- >>> parseNumber "133t"
-- Nothing

parseNumber :: String -> Maybe Integer
parseNumber str =
  let parsedDigits = map parseDigit str
  in case validateList parsedDigits of
    Just digits -> Just (digitsToNumber digits)
    Nothing -> Nothing

-- EXERCISE
-- Notice how in parseNumber, in the Nothing case we returned Nothing,
-- and in the Just case, we returned Just again, with a "non-maybe" function inside.
-- This turns out to be very useful, and if you compare it to the @map@ for lists, it's almost the same.
-- Let's write it now, so we don't have to do that pattern match again in the future.
-- Afterwards, you can reuse this function in parseNumber.
-- EXAMPLES
-- >>> maybeMap succ $ Just 5
-- Just 6
-- >>> maybeMap succ Nothing
-- Nothing

maybeMap :: (a -> b) -> Maybe a -> Maybe b
maybeMap _ Nothing = Nothing
maybeMap f (Just x) = Just (f x)

-- EXERCISE
-- Reverse a list using foldl.
-- It might help to implement the "accumulating with a helper" version first, if you haven't already
-- What's the complexity for reverse'?
-- >>> reverse' [1,2,3]
-- [3,2,1]

reverse :: [a] -> [a]
reverse xs = foldl (flip (:)) [] xs

-- EXERCISE
-- A smaller version of one of the tasks from a FP exam.
-- We have instructions for a "stack machine" - so something that keeps a stack for memory (so a list)
-- Push n is meant to push the value n on the stack
-- Map f is meant to apply f to *all* of the items in the stack in place, so without removing them
-- Oper f is meant to pop the top two items from the stack, apply f to them, and push the result back on the stack

data Instruction
  = Push Integer
  | Map (Integer -> Integer)
  | Oper (Integer -> Integer -> Integer)

-- Implement the interpreter for our stack machine, given a list of instructions.
-- You can use a helper for the "recursion" at first, if you want to, but implement the "recursive part" using foldl afterwards.
-- We return the final state of the stack.
-- Note the @Maybe@, because we can't be sure that @Oper@ will always succeed, because we might not have two arguments in our stack.
-- We'll consider this failure a critical error, and won't attempt to recover from a failure.
-- EXAMPLES
-- >>> runMachine [Push 9, Push 6]
-- Just [6,9]
-- >>> runMachine [Oper (+)]
-- Nothing
-- >>> runMachine [Push 42, Oper (+)]
-- Nothing
-- >>> runMachine [Push 42, Push 69, Oper (+)]
-- Just [111]
-- >>> runMachine [Push 42, Oper (+), Push 69]
-- Nothing
-- >>> runMachine [Push 7, Push 2, Map (\x -> x * x), Push 5]
-- Just [5,4,49]
-- >>> runMachine [Push 7, Push 2, Map (\x -> x * x), Push 5, Oper (*), Oper (+)]
-- Just [69]
-- >>> runMachine [Push 7, Push 2, Oper (+), Oper (+)]
-- Nothing

runMachine :: [Instruction] -> Maybe [Integer]
runMachine instructions = foldl step (Just []) instructions
  where
    step :: Maybe [Integer] -> Instruction -> Maybe [Integer]
    step (Just stack) (Push x) = Just (x : stack)
    step (Just stack) (Map f) = Just (map f stack)
    step (Just (x : y : stack)) (Oper f) = Just ((f x y) : stack)
    step _ (Oper _) = Nothing -- not enough elements in stack
    step Nothing _ = Nothing

-- EXERCISE
-- Look at the recursor for @Nat@s - @foldNat@. In there we replaced @Nat@'s constructors with "things".
-- Think about how a fold for tuples should look like, and implement it.
-- Does this function remind you of another function we've previously implemented?
-- foldTuple :: ?
-- foldTuple = undefined

-- EXERCISE
-- Same as above, but this time for Maybe
-- foldMaybe :: ?
-- foldMaybe = undefined

-- EXERCISE
-- Same as above, but this time for Either
-- Reminder: Either is defined like so:
-- data Either a b = Left a | Right b
--
-- foldEither :: ?
-- foldEither = undefined

-- EXERCISEs
-- A (binary) tree is either empty, or it has an element (a root) along with a left and right subtree
data Tree a
  = Empty
  | MkTree (Tree a) a (Tree a)
  deriving (Eq, Show)

-- A function to construct leaves (trees whose left and right subtree are Empty) more easily.
isEmpty :: Tree a -> Bool
isEmpty Empty = True
isEmpty _ = False

leaf :: a -> Tree a
leaf x = MkTree Empty x Empty

isLeaf :: Tree a -> Bool
isLeaf Empty = False
isLeaf (MkTree lhs _ rhs) = isEmpty lhs && isEmpty rhs

-- EXERCISE
-- Find the depth of a tree
-- EXAMPLES
-- >>> depth Empty
-- 0
-- >>> depth $ leaf 5
-- 1
-- >>> depth (MkTree (leaf 5) 6 Empty)
-- 2
-- >>> depth (MkTree (leaf 5) 6 (MkTree (leaf 7) 8 Empty))
-- 3

depth :: Tree a -> Integer
depth Empty = 0
depth (MkTree lhs _ rhs) = 1 + max (depth lhs) (depth rhs)

-- EXERCISE
-- Reverse a tree
-- EXAMPLES
-- >>> reverseTree $ leaf 5
-- MkTree Empty 5 Empty
-- >>> reverseTree $ MkTree (leaf 5) 6 (leaf 7)
-- MkTree (MkTree Empty 7 Empty) 6 (MkTree Empty 5 Empty)

reverseTree :: Tree a -> Tree a
reverseTree Empty = Empty
reverseTree (MkTree lhs x rhs) = MkTree (reverseTree rhs) x (reverseTree lhs)

-- EXERCISE
-- Think about what a "fold" for a Tree would be and implement it.
-- Remember - you need to look at the constructors.

foldTree :: (b -> a -> b -> b) -> b -> Tree a -> b
foldTree _ acc Empty = acc
foldTree f acc (MkTree lhs x rhs) = f (foldTree f acc lhs) x (foldTree f acc rhs)

-- EXERCISE
-- Find the depth of a tree using foldTree
-- EXAMPLES
-- >>> depth' Empty
-- 0
-- >>> depth' $ leaf 5
-- 1
-- >>> depth' (MkTree (leaf 5) 6 Empty)
-- 2
-- >>> depth' (MkTree (leaf 5) 6 (MkTree (leaf 7) 8 Empty))
-- 3

depth' :: Tree a -> Integer
depth' = foldTree (\lhsDepth _ rhsDepth -> 1 + max lhsDepth rhsDepth) 0

-- EXERCISE
-- Reverse a tree using foldTree
-- EXAMPLES
-- >>> reverseTree' $ leaf 5
-- MkTree Empty 5 Empty
-- >>> reverseTree' $ MkTree (leaf 5) 6 (leaf 7)
-- MkTree (MkTree Empty 7 Empty) 6 (MkTree Empty 5 Empty)

reverseTree' :: Tree a -> Tree a
reverseTree' = foldTree (\lhs x rhs -> (MkTree rhs x lhs)) Empty

-- EXERCISE
-- Map over a tree using foldTree
-- EXAMPLES
-- >>> mapTree succ $ leaf 5
-- MkTree Empty 6 Empty
-- >>> mapTree (\x -> x * x) $ MkTree (leaf 5) 6 (leaf 7)
-- MkTree (MkTree Empty 25 Empty) 36 (MkTree Empty 49 Empty)

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f = foldTree (\lhs x rhs -> (MkTree lhs (f x) rhs)) Empty

-- | EXERCISE
-- Insert into a binary search tree, keeping the "ordered" property.
-- BST definition:
-- Empty is a BST
-- MkTree l x r is a BST if
-- * l is a BST
-- * r is a BST
-- * x is larger than all of the elements in l
-- * x is smaller than all of the elements in r
-- EXAMPLES
-- >>> insert 5 Empty
-- MkTree Empty 5 Empty
-- >>> insert 5 $ leaf 6
-- MkTree (MkTree Empty 5 Empty) 6 Empty
-- >>> insert 5 $ leaf 4
-- MkTree Empty 4 (MkTree Empty 5 Empty)
-- >>> insert 5 $ (MkTree (leaf 4) 7 (leaf 8))
-- MkTree (MkTree Empty 4 (MkTree Empty 5 Empty)) 7 (MkTree Empty 8 Empty)

insert :: Integer -> Tree Integer -> Tree Integer
insert value Empty = (MkTree Empty value Empty)
insert value (MkTree lhs x rhs)
  | value < x = MkTree (insert value lhs) x rhs
  | otherwise = MkTree lhs x (insert value rhs)

-- EXERCISE
-- Use foldr to convert a list into a BST.
-- EXAMPLES
-- >>> listToTree [2,1,3]
-- MkTree (MkTree Empty 1 (MkTree Empty 2 Empty)) 3 Empty
-- >>> listToTree [1,2,3]
-- MkTree (MkTree (MkTree Empty 1 Empty) 2 Empty) 3 Empty
-- >>> listToTree [4,1,2,5]
-- MkTree (MkTree (MkTree Empty 1 Empty) 2 (MkTree Empty 4 Empty)) 5 Empty

listToTree :: [Integer] -> Tree Integer
listToTree = foldr (\x acc -> insert x acc) Empty

-- EXERCISE
-- Use foldTree to convert a tree into a list. You should be walking the tree in a "left root right" order.
-- EXAMPLES
-- >>> treeToList $ leaf 5
-- [5]
-- >>> treeToList $ (MkTree (leaf 6) 5 (leaf 3))
-- [6,5,3]
-- >>> treeToList $ (MkTree (MkTree (leaf 7) 8 (leaf 69)) 5 (leaf 3))
-- [7,8,69,5,3]

treeToList :: Tree a -> [a]
treeToList = foldTree (\lhs x rhs -> lhs ++ [x] ++ rhs) []

-- EXERCISE
-- Sort a list using the above two functions.
-- Do the operations that this sort executes remind you of some other sorting technique?
-- EXAMPLES
-- >>>

sort :: [Integer] -> [Integer]
sort xs = treeToList $ listToTree xs

-- EXERCISE
-- Use foldTree to check if an element is in a tree, assuming it's a BST.
-- EXAMPLES
-- >>> searchTree 5 $ listToTree [1,2,3]
-- False
-- >>> searchTree 2 $ listToTree [1,2,3]
-- True

searchTree :: Integer -> Tree Integer -> Bool
searchTree value = foldTree (\lhs x rhs -> lhs || (x == value) || rhs) False

-- EXERCISE
-- Use foldTree to delete all the occurences of an element from a tree.
-- In the case that the root "disappears", it's fine to merge the left tree into the right one (or vice versa).
-- I chose merging the left tree into the right one, so the examples might not be exactly the same for you,
-- if you choose the other direction. (or another strategy entirely to deal with missing roots)
-- EXAMPLES
-- >>> deleteTree 5 $ listToTree [5,5,5]
-- Empty
-- >>> deleteTree 5 $ listToTree [1,5,5,8,6]
-- MkTree (MkTree Empty 1 Empty) 6 (MkTree Empty 8 Empty)
-- >>> deleteTree 5 $ listToTree [1,5,67,5,8]
-- MkTree (MkTree Empty 1 Empty) 8 (MkTree Empty 67 Empty)

findMin :: (Integral a) => Tree a -> a
findMin (MkTree Empty x _) = x
findMin (MkTree lhs _ _) = findMin lhs

removeRoot :: (Integral a) => Tree a -> Tree a
removeRoot (MkTree Empty _ rhs) = rhs
removeRoot (MkTree lhs _ Empty) = lhs
removeRoot (MkTree lhs _ rhs) =
  let next = findMin rhs
  in MkTree lhs next (deleteTree next rhs)

deleteTree :: (Integral a) => a -> Tree a -> Tree a
deleteTree _ Empty = Empty
deleteTree value (MkTree lhs x rhs)
  | x == value = removeRoot (MkTree lhs value rhs)
  | value < x = MkTree (deleteTree value lhs) x rhs
  | value > x = MkTree lhs x (deleteTree value rhs)

-- EXERCISE
-- Trees also admit "a foldr"
-- Is there more than one way to write a foldr for trees? What's the difference?
foldrTree :: (a -> b -> b) -> b -> Tree a -> b
foldrTree = undefined

-- EXERCISE
-- Sum a tree using foldrTree

sumTree :: (Integral a) => Tree a -> a
sumTree = foldTree (\lhs x rhs -> lhs + x + rhs) 0

-- EXERCISE
-- Convert a tree into a list using foldrTree
treeToList' :: Tree a -> [a]
treeToList' = undefined

data Nat
  = Zero
  | Suc Nat
  deriving (Show)

-- EXERCISE
-- If Nats can be converted to "n times applications" via foldNat,
-- is it perhaps true that "n times applications" can also be converted to Nats somehow?
--
-- You can ignore this "forall explanation" bit below if you want to - just assume the forall means "the passed function must be polymorphic over a"
-- START "forall explanation"
-- Usually when we have a polymorphic function, like id :: a -> a
-- the *caller* chooses what a will be - when the caller writes @id 'a'@, they instantiate @a@ with @Char@, so @id@ becomes @id :: Char -> Char@
-- However, here we will need our function to work for any @a@, and so we must *require* something of the caller -
-- that they provide a function working *for any* @a@ - meaning *we*(the callee) can decide what @a@ to apply it for, reversing who can pick what the type is.
--
-- As a concrete example, consider
-- @
-- f :: (a -> a) -> Bool
-- f g = g True
-- @
-- this does *not* compile - let's assume it did:
-- If we have
-- @
-- h :: Int -> Int
-- h x = x + 1
-- @
-- then the caller would be able to write @f h@, (as they pick what @a@ is) which is not valid,
-- since @h@ requires its argument and return types to be @Int@, and @True :: Bool@, which is not @Int@
-- If we instead consider
-- @
-- f :: (forall a. a -> a) -> Bool
-- f g = g True
-- @
-- we, as implementors of @f@, are now allowed to pick what @a@ to use, hence we are allowed to pick @a ~ Bool@,
-- by calling @g@ with @True :: Bool@
-- Hence, this function now compiles, and furthermore, if our hypothetical caller now attempts to do
-- @f h@
-- They will get a compilation error, since the argument to @f@ needs to work *for any* type, while @h@ has the concrete type @Int -> Int@
-- END "forall explanation"
--
-- EXAMPLES
-- >>> iterateToNat (\f x -> f (f (f x)))
-- Suc (Suc (Suc Zero))
iterateToNat :: (forall a. (a -> a) -> a -> a) -> Nat
iterateToNat _f = undefined

-- EXERCISE
-- This is the same as foldNat, except with arguments reaarranged to mirror @iterateToNat@
natToIterate :: Nat -> (a -> a) -> a -> a
natToIterate = undefined

-- | EXERCISE
-- Hey, if we can convert between Natural (the type argument to @iterateToNat@, now with a synonym) and @Nat@ without losing information
-- wouldn't that mean that they are equivalent, and we can do the same things with both?
-- Let's reimplement some of the operations over @Nat@ with @Natural@ instead:
type Natural = forall a. (a -> a) -> a -> a

-- These are called "church encoded" natural numbers - they're used to represent natural numbers when the only thing you have is functions.
--
-- Here's some exposition:
-- As you saw in the @iterateToNat@ example, these Naturals are essentially applying some function to some value a number of times.
-- The idea is that we represent the number @n@ as applying a function @f@ @n@ times to a value @v@.
-- For example:
-- 0 is represented by \f v -> v
zero :: Natural
zero _f v = v

-- 1 is represented by \f v -> f v
-- 2 is represented by \f v -> f (f v)
-- 3 is represented by \f v -> f (f (f v))
-- and so on
-- With this function, we need to somehow "add another f".
-- EXAMPLES
-- >>> iterateToNat zero
-- Zero
-- >>> iterateToNat $ suc $ suc zero
-- Suc (Suc Zero)
-- >>> natToInteger $ iterateToNat $ suc $ natToIterate $ integerToNat 5
-- 6
suc :: Natural -> Natural
suc _n = undefined

-- EXERCISE
-- We can also add these. Here we need to think about how to add f n times to another Natural.
-- EXAMPLES
-- >>> iterateToNat $ add (suc (suc zero)) zero
-- Suc (Suc Zero)
-- >>> iterateToNat $ add (suc (suc zero)) (suc (suc (suc zero)))
-- Suc (Suc (Suc (Suc (Suc Zero))))
-- >>> natToInteger $ iterateToNat $ add (suc (suc zero)) (suc (suc (suc (suc zero))))
-- 6
add :: Natural -> Natural -> Natural
add _n _m = undefined

-- EXERCISE
-- Now multiply them
-- >>> iterateToNat $ mult (suc (suc zero)) zero
-- Zero
-- >>> iterateToNat $ mult zero (suc (suc zero))
-- Zero
-- >>> iterateToNat $ mult (suc (suc zero)) (suc (suc zero))
-- Suc (Suc (Suc (Suc Zero)))
-- >>> natToInteger $ iterateToNat $ mult (suc (suc zero)) (suc (suc (suc zero)))
-- 6
mult :: Natural -> Natural -> Natural
mult _n _m = undefined

-- Is the same true for lists? Is there some function type that is "isomorphic" to lists - you can convert
-- back and forth between lists and the function, without losing data? Like how Natural is to Nat
-- (or if you prefer - can you express lists by only using lambdas?)

-- EXERCISE
-- foldr is more general than foldl. Indeed, we can even implement foldl using foldr
-- HINT: We're going to be constructing a function, which we then apply to our initial value v.
-- We want the function to emulate what foldl f v xs would normally do.
-- id and (.) are going to be useful.
-- It mighto also help to think about, if f' = flip f, how you can transform
-- f' x (f' y v))
-- into
-- f (f v x) y
-- Use type holes - if you write _ in a place where you want an argument, you'll get an error explaining what the type
-- that's expected there is.
foldlViaFoldr :: forall a b. (b -> a -> b) -> b -> [a] -> b
foldlViaFoldr = undefined