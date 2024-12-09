{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# OPTIONS_GHC -fwarn-unused-matches #-}
{-# OPTIONS_GHC -fwarn-missing-signatures #-}
{-# OPTIONS_GHC -fwarn-name-shadowing #-}
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Werror #-}

myConcat :: (Integral a) => a -> a -> a
myConcat n digit = (n * 10) + digit

reverseNumber :: (Integral a) => a -> a
reverseNumber x = reverseNumber' x 0
    where
        reverseNumber' :: (Integral a) => a -> a -> a
        reverseNumber' 0 acc = acc
        reverseNumber' n acc = 
            let lastDigit = n `mod` 10
            in reverseNumber' (n `div` 10) (myConcat acc lastDigit)

-- reverseNumber'' :: (Integral a, Read a, Show a) => a -> a
-- reverseNumber'' = read . reverse . show

addOneN :: (Integral a) => a -> a
addOneN x = x + 1

addOneXS :: (Integral a) => [a] -> [a]
addOneXS xs = [(addOneN x) | x <- xs]

sq :: (Integral a) => a -> a
sq x = x^2

sqPlusOne :: (Integral a) => a -> a
sqPlusOne x = addOneN (sq x)

