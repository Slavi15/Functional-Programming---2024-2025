module Binary where

data Binary
  = End
  | Binary :. Bit
  deriving (Show)

data Bit = Zero | One
  deriving (Show)

infixl 6 :.

succBinary :: Binary -> Binary
succBinary End = End :. One
succBinary (x :. Zero) = x :. One
succBinary (x :. One) = succBinary x :. Zero

integerToBinary :: Integer -> Binary
integerToBinary 0 = End
integerToBinary x = integerToBinary (x `div` 2) :. (if x `mod` 2 == 0 then Zero else One)

bitToInteger :: Bit -> Integer
bitToInteger Zero = 0
bitToInteger One = 1

integerToBit :: Integer -> Bit
integerToBit 0 = Zero
integerToBit 1 = One
integerToBit _ = Zero

binaryToInteger :: Binary -> Integer
binaryToInteger End = 0
binaryToInteger (x :. xs) = ((binaryToInteger x) * 2) + (bitToInteger xs)

hasLeadingZero :: Binary -> Bool
hasLeadingZero End = False
hasLeadingZero (End :. Zero) = True
hasLeadingZero (End :. One) = False
hasLeadingZero (x :. _) = hasLeadingZero x

isEnd :: Binary -> Bool
isEnd End = True
isEnd _ = False

canonicalise :: Binary -> Binary
canonicalise End = End
canonicalise (x :. Zero) = 
  case canonicalise x of
    End -> End
    x' -> x' :. Zero
canonicalise (x :. One) = canonicalise x :. One

eqBit :: Bit -> Bit -> Bool
eqBit Zero Zero = True
eqBit One One = True
eqBit _ _ = False

addBinary :: Binary -> Binary -> Binary
addBinary = loop Zero
  where
    loop :: Bit -> Binary -> Binary -> Binary
    loop carry End End = if eqBit carry Zero then End else (End :. carry)
    loop carry (x :. xs) End =
      let (newBit, newCarry) = addBits (bitToInteger xs + bitToInteger carry)
      in loop newCarry x End :. newBit
    loop carry End (y :. ys) =
      let (newBit, newCarry) = addBits (bitToInteger ys + bitToInteger carry)
      in loop newCarry End y :. newBit
    loop carry (x :. xs) (y :. ys) =
      let (newBit, newCarry) = addBits (bitToInteger xs + bitToInteger ys + bitToInteger carry)
      in loop newCarry x y :. newBit

    addBits summation = (integerToBit (summation `mod` 2), integerToBit (summation `div` 2))