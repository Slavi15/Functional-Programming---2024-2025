{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -fwarn-missing-signatures #-}
{-# OPTIONS_GHC -fwarn-name-shadowing #-}
{-# OPTIONS_GHC -fwarn-unused-matches #-}

checkID :: [Float -> Float] -> [Float] -> Bool 
checkID fs xs = any match [f . g | f <- fs, g <- fs]
    where
        match func = all (\x -> x == func x) xs

-- >>> checkID [(^2), sin, (+2), sqrt] [1 .. 5] -- => True
-- >>> checkID [abs, max 2, min 4] [-1 .. 5] -- => False
-- True
-- False
