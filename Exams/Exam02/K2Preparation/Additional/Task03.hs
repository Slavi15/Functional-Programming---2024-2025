ones :: [Int]
ones = 1 : ones

twos :: [Int]
twos = 2 : twos

threes :: [Int]
threes = 3 : threes

braidStreams :: [a] -> [a] -> [a] -> ([a], [a], [a])
braidStreams (a : as) (b : bs) (c : cs) =
    let (nextA, nextB, nextC) = braidStreams' as bs cs 0
    in (a : nextA, b : nextB, c : nextC)
    
    where
        braidStreams' :: [a] -> [a] -> [a] -> Integer -> ([a], [a], [a])
        braidStreams' (a' : as') (b' : bs') (c' : cs') pos 
            | even pos =
                let (nextA, nextB, nextC) = braidStreams' bs' as' cs' (pos + 1)
                in (b' : nextA, a' : nextB, c' : nextC)
            | otherwise =
                let (nextA, nextB, nextC) = braidStreams' as' cs' bs' (pos + 1)
                in (a' : nextA, c' : nextB, b' : nextC)

x :: [Int]
y :: [Int]
z :: [Int]
(x, y, z) = braidStreams ones twos threes

-- >>> take 10 $ x
-- >>> take 10 $ y
-- >>> take 10 $ z
-- [1,2,2,3,3,1,1,2,2,3]
-- [2,1,3,2,1,3,2,1,3,2]
-- [3,3,1,1,2,2,3,3,1,1]
