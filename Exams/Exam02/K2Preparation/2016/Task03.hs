{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# OPTIONS_GHC -fwarn-unused-matches #-}
{-# OPTIONS_GHC -fwarn-missing-signatures #-}
{-# OPTIONS_GHC -fwarn-name-shadowing #-}
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Werror #-}

import Data.List (nub, sortBy, maximumBy)

type Plant = (String, Int, Int)

maximumComparator :: (Foldable t1, Foldable t2, Ord a1) => (a1, t1 a2) -> (a1, t2 a3) -> Ordering
maximumComparator (_, plts1) (_, plts2)
    | length plts1 < length plts2 = LT
    | length plts1 > length plts2 = GT
    | otherwise = EQ

garden :: [Plant] -> ((Int, Int), [String])
garden plants' =
    let
        allTemps = nub $ concatMap (\(_, minT, maxT) -> [minT .. maxT]) plants'

        tempPlants = 
            [(temp, 
            [name | (name, minT, maxT) <- plants', temp >= minT && temp <= maxT]) 
            | temp <- allTemps]

        temps = sortBy maximumComparator tempPlants
        maximumElement = maximumBy maximumComparator temps

        filtered = filter (\(_, plants'') -> all (`elem` plants'') (snd maximumElement)) temps

        minTemp = fst $ head filtered
        maxTemp = fst $ last filtered

        bestRange = (minTemp, maxTemp)
        bestPlants = snd $ head filtered
    in (bestRange, bestPlants)

plants :: [Plant]
plants = [("peas", 5, 25), ("beans", 3, 15), ("cocoa", 20, 30)]

-- >>> garden plants
-- ((20,25),["peas","cocoa"])
