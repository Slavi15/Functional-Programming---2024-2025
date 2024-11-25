(!=) :: (Eq a) => a -> a -> Bool
x != y = x /= y

isValidDate :: Int -> Int -> Int -> Bool
isValidDate day month year
    | (month < 1) || (month > 12) = False
    | (day < 1) || (day > (getMonthDays month year)) = False
    | otherwise = True

isLeapYear :: Int -> Bool
isLeapYear year
    | (year `mod` 4 == 0 && year `mod` 100 /= 0) || (year `mod` 400 == 0) = True
    | otherwise = False

getMonthDays :: Int -> Int -> Int
getMonthDays month year
    | (month == 1) || (month == 3) || (month == 5) || (month == 7) || (month == 8) || (month == 10) || (month == 12) = 31
    | (month == 4) || (month == 6) || (month == 9) || (month == 11) = 30
    | otherwise = if (isLeapYear year) then 29 else 28

countDays :: Int -> Int -> Int -> Int
countDays day month year = 
    if isValidDate day month year 
    then countDays' day month 0
    else 0
    
    where
        countDays' :: Int -> Int -> Int -> Int
        countDays' _ 0 count = count
        countDays' n m count = countDays' (getMonthDays (m - 1) year) (m - 1) (count + n)