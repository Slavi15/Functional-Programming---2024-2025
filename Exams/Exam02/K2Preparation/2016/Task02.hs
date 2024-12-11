{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# OPTIONS_GHC -fwarn-unused-matches #-}
{-# OPTIONS_GHC -fwarn-missing-signatures #-}
{-# OPTIONS_GHC -fwarn-name-shadowing #-}
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Werror #-}

combine :: (Integral a) => (a -> a) -> (a -> a) -> (a -> a -> a) -> a -> a
combine f g h x = h (f x) (g x)

check :: (Integral a) => a -> a -> [a -> a] -> [a -> a -> a] -> Bool
check a b uns bins = 
    any match [combine f g h | f <- uns, g <- uns, h <- bins]
    where
        match func = any (\unsFunc -> all (\x -> func x == unsFunc x) [a .. b]) uns

-- >>> check 1 9 [(+1),(\x -> x - 1),(\x -> x - 1).(^2)] [(*)] -- => True
-- True
