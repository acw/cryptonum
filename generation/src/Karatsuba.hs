{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module Karatsuba(
         Instruction(..)
       , runChecks
       , runQuickCheck
       , generateInstructions
       )
 where

import Control.Monad.Fail(MonadFail(..))
import Control.Monad.Identity hiding (fail)
import Control.Monad.RWS.Strict hiding (fail)
import Data.Bits
import Data.LargeWord
import Data.List
import Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map
import Data.Vector(Vector, (!?))
import qualified Data.Vector as V
import Data.Word
import Prelude hiding (fail)
import Test.QuickCheck hiding ((.&.))

-- this drives the testing
inputWordSize :: Int
inputWordSize = 5

generateInstructions :: Word -> [Instruction]
generateInstructions numdigits = foldl replaceVar baseInstrs varRenames
 where
  x = rename "x" (V.replicate (fromIntegral numdigits) (D "" 1))
  y = rename "y" (V.replicate (fromIntegral numdigits) (D "" 1))
  (baseVec, baseInstrs) = runMath (karatsuba x y)
  res = rename "res" baseVec
  varRenames = zip (map name (V.toList baseVec)) (map name (V.toList res))

-- -----------------------------------------------------------------------------
--
-- Instructions that we emit as a result of running Karatsuba, that can be
-- turned into Rust lines.
--
-- -----------------------------------------------------------------------------

-- these are in Intel form, as I was corrupted young, so the first argument
-- is the destination and the rest are the arguments.
data Instruction = Add        String [String]
                 | CastDown   String String
                 | CastUp     String String
                 | Complement String String
                 | Declare64  String Word64
                 | Declare128 String Word128
                 | Mask       String String   Word128
                 | Multiply   String [String]
                 | ShiftR     String String   Int
 deriving (Eq, Show)

class Declarable a where
    declare :: String -> a -> Instruction

instance Declarable Word64 where
    declare n x = Declare64  n x
instance Declarable Word128 where
    declare n x = Declare128 n x

type Env = (Map String Word64, Map String Word128)

step :: Env -> Instruction -> Env
step (env64, env128) i =
  case i of
    Add        outname items      ->
      (env64, Map.insert outname (sum (map (getv env128) items)) env128)
    CastDown   outname item       ->
      (Map.insert outname (fromIntegral (getv env128 item)) env64, env128)
    CastUp     outname item       ->
      (env64, Map.insert outname (fromIntegral (getv env64 item)) env128)
    Complement outname item       ->
      (Map.insert outname (complement (getv env64 item)) env64, env128)
    Declare64  outname val        ->
      (Map.insert outname val env64, env128)
    Declare128 outname val        ->
      (env64, Map.insert outname val env128)
    Mask       outname item  mask ->
      (env64, Map.insert outname (getv env128 item .&. mask) env128)
    Multiply   outname items      ->
      (env64, Map.insert outname (product (map (getv env128) items)) env128)
    ShiftR     outname item  amt  ->
      (env64, Map.insert outname (getv env128 item `shiftR` amt) env128)
 where
  getv env s =
    case Map.lookup s env of
      Nothing ->
        error ("Failure to find key '" ++ s ++ "'")
      Just v ->
        v

run :: Env -> [Instruction] -> Env
run env instrs =
  case instrs of
    []       -> env
    (x:rest) -> run (step env x) rest

replaceVar :: [Instruction] -> (String, String) -> [Instruction]
replaceVar ls (from, to) = map replace ls
 where
  replace x =
    case x of
      Add        outname items      -> Add        (sub outname) (map sub items)
      CastDown   outname item       -> CastDown   (sub outname) (sub item)
      CastUp     outname item       -> CastUp     (sub outname) (sub item)
      Complement outname item       -> Complement (sub outname) (sub item)
      Declare64  outname val        -> Declare64  (sub outname) val
      Declare128 outname val        -> Declare128 (sub outname) val
      Mask       outname item  mask -> Mask       (sub outname) (sub item) mask
      Multiply   outname items      -> Multiply   (sub outname) (map sub items)
      ShiftR     outname item  amt  -> ShiftR     (sub outname) (sub item) amt
  sub x | x == from = to
        | otherwise = x

-- -----------------------------------------------------------------------------
--
-- The Math monad.
--
-- -----------------------------------------------------------------------------

newtype Math a = Math { unMath :: RWS () [Instruction] Integer a }
 deriving (Applicative, Functor, Monad,
           MonadState Integer,
           MonadWriter [Instruction])

instance MonadFail Math where
  fail s = error ("Math fail: " ++ s)

emit :: Instruction -> Math ()
emit instr = tell [instr]

gensym :: String -> Math String
gensym base =
  do x <- state (\ i -> (i, i + 1))
     return (base ++ show x)

runMath :: Math a -> (a, [Instruction])
runMath m = evalRWS (unMath m) () 0

-- -----------------------------------------------------------------------------
--
-- Primitive mathematics that can run on a Digit
--
-- -----------------------------------------------------------------------------

data Digit size = D  {
   name     :: String
 , digit    :: size
 }
 deriving (Eq,Show)

genDigit :: Declarable size => String -> size -> Math (Digit size)
genDigit nm x =
  do newName <- gensym nm
     emit (declare newName x)
     return D{
       name  = newName
     , digit = x
     }

embiggen :: Digit Word64 -> Math (Digit Word128)
embiggen x =
  do newName <- gensym ("big_" ++ name x)
     emit (CastUp newName (name x))
     return (D newName (fromIntegral (digit x)))

bottomBits :: Digit Word128 -> Math (Digit Word64)
bottomBits x =
  do newName <- gensym ("norm_" ++ name x)
     emit (CastDown newName (name x))
     return (D newName (fromIntegral (digit x)))

oneDigit :: Math (Digit Word64)
oneDigit = genDigit "one" 1

bigZero :: Math (Digit Word128)
bigZero = genDigit "zero" 0

(|+|) :: Digit Word128 -> Digit Word128 -> Math (Digit Word128)
(|+|) x y =
  do newName <- gensym "plus"
     emit (Add newName [name x, name y])
     let digval = digit x + digit y
     return (D newName digval)

sumDigits :: [Digit Word128] -> Math (Digit Word128)
sumDigits ls =
  do newName <- gensym "sum"
     emit (Add newName (map name ls))
     let digval = sum (map digit ls)
     return (D newName digval)

(|*|) :: Digit Word128 -> Digit Word128 -> Math (Digit Word128)
(|*|) x y =
  do newName <- gensym "times"
     emit (Multiply newName [name x, name y])
     let digval = digit x * digit y
     return (D newName digval)

(|>>|) :: Digit Word128 -> Int -> Math (Digit Word128)
(|>>|) x s =
  do newName <- gensym "shiftr"
     emit (ShiftR newName (name x) s)
     let digval = digit x `shiftR` s
     return (D newName digval)

(|&|) :: Digit Word128 -> Word128 -> Math (Digit Word128)
(|&|) x m =
  do newName <- gensym ("masked_" ++ name x)
     emit (Mask newName (name x) m)
     let digval = digit x .&. m
     return (D newName digval)

complementDigit :: Digit Word64 -> Math (Digit Word64)
complementDigit x =
  do newName <- gensym ("comp_" ++ name x)
     emit (Complement newName (name x))
     return (D newName (complement (digit x)))

-- -----------------------------------------------------------------------------
--
-- Extended mathematics that run on whole numbers
--
-- -----------------------------------------------------------------------------

type Number = Vector (Digit Word64)

instance Arbitrary Number where
  arbitrary =
    do ls <- replicateM inputWordSize (D "" <$> arbitrary)
       return (V.fromList ls)

rename :: String -> Number -> Number
rename var num = go 0 num
 where
  go :: Word -> Number -> Number
  go i v =
    case v !? 0 of
      Nothing -> V.empty
      Just x  -> D (var ++ show i) (digit x) `V.cons` go (i + 1) (V.drop 1 v)

convertTo :: Int -> Integer -> Number
convertTo s = pad . V.unfoldrN s next
 where
  next 0 = Nothing
  next x = Just (D{ name = "", digit = fromIntegral x }, x `shiftR` 64)
  pad v | V.length v == s = v
        | otherwise       = pad (v <> V.singleton D{ name = "", digit = 0})

convertFrom :: Number -> Integer
convertFrom n = V.foldr combine 0 n
 where
  combine x acc = (acc `shiftL` 64) + fromIntegral (digit x)

prop_ConversionWorksNum :: Number -> Bool
prop_ConversionWorksNum n =
  n == convertTo inputWordSize (convertFrom n)

prop_ConversionWorksInt :: Integer -> Bool
prop_ConversionWorksInt n =
  n' == convertFrom (convertTo inputWordSize n)
 where n' = n `mod` (2 ^ (inputWordSize * 64))

zero :: Int -> Math Number
zero s = V.fromList `fmap` replicateM s (genDigit "zero" 0)

empty :: Number -> Bool
empty = null

size :: Number -> Int
size = length

splitDigits :: Int -> Number -> Math (Number, Number)
splitDigits i ls = return (V.splitAt i ls)

prop_SplitDigitsIsntTerrible :: Int -> Number -> Bool
prop_SplitDigitsIsntTerrible x n =
  let ((left, right), _) = runMath (splitDigits x' n)
  in n == (left <> right)
 where x' = x `mod` inputWordSize

addZeros :: Int -> Number -> Math Number
addZeros x n =
  do prefix <- zero x
     return (prefix <> n)

prop_AddZerosIsShift :: Int -> Number -> Bool
prop_AddZerosIsShift x n =
  let x' = abs (x `mod` inputWordSize)
      nInt = convertFrom n
      shiftVersion = nInt `shiftL` (x' * 64)
      addVersion = convertFrom (fst (runMath (addZeros x' n)))
  in shiftVersion == addVersion

padTo :: Int -> Number -> Math Number
padTo len num =
  do suffix <- zero (len - V.length num)
     return (num <> suffix)

prop_PadToWorks :: Int -> Number -> Property
prop_PadToWorks len num = len >= size num ==>
  convertFrom num == convertFrom (fst (runMath (padTo len num)))

add2 :: Number -> Number -> Math Number
add2 xs ys
  | size xs /= size ys =
      fail "Add2 of uneven vectors."
  | otherwise =
      do let both = V.zip xs ys
         nada <- bigZero
         (res, carry) <- foldM ripple (V.empty, nada) both
         lastDigit <- bottomBits carry
         return (res <> V.singleton lastDigit)
 where
  ripple (res, carry) (x, y) =
    do x'        <- embiggen x
       y'        <- embiggen y
       bigRes    <- sumDigits [x', y', carry]
       carry'    <- bigRes |>>| 64
       newdigit  <- bottomBits bigRes
       let res' = res <> V.singleton newdigit
       return (res', carry')

prop_Add2Works :: Number -> Number -> Bool
prop_Add2Works n m =
  let nInt = convertFrom n
      mInt = convertFrom m
      intRes = nInt + mInt
      (numRes, _) = runMath (add2 n m)
      numResInt = convertFrom numRes
  in (size numRes == inputWordSize + 1) && (intRes == numResInt)

add3 :: Number -> Number -> Number -> Math Number
add3 x y z
  | size x /= size y =
      fail "Unequal lengths in add3 (1)."
  | size y /= size z =
      fail "Unequal lengths in add3 (2)."
  | otherwise =
      do let allThem = V.zip3 x y z
         nada <- bigZero
         (res, carry) <- foldM ripple (V.empty, nada) allThem
         lastDigit <- bottomBits carry
         return (res <> V.singleton lastDigit)
 where
  ripple (res, carry) (a, b, c) =
    do a'     <- embiggen a
       b'     <- embiggen b
       c'     <- embiggen c
       bigRes <- sumDigits [a', b', c', carry]
       carry' <- bigRes |>>| 64
       digit' <- bottomBits bigRes
       let res' = res <> V.singleton digit'
       return (res', carry')

prop_Add3Works :: Number -> Number -> Number -> Bool
prop_Add3Works x y z =
  let xInt = convertFrom x
      yInt = convertFrom y
      zInt = convertFrom z
      intRes = xInt + yInt + zInt
      (numRes, _) = runMath (add3 x y z)
      numResInt = convertFrom numRes
  in (size numRes == inputWordSize + 1) && (intRes == numResInt)

sub2 :: Number -> Number -> Math Number
sub2 x y
  | size x /= size y =
      fail "Unequal lengths in sub."
  | otherwise =
      do yinv <- mapM complementDigit y
         oned <- oneDigit
         one  <- padTo (size x) (V.singleton oned)
         res  <- add3 x yinv one
         return (V.take (size x) res)

prop_Sub2Works :: Number -> Number -> Bool
prop_Sub2Works a b
  | convertFrom a < convertFrom b = prop_Sub2Works b a
  | otherwise =
      let aInt = convertFrom a
          bInt = convertFrom b
          intRes = aInt - bInt
          (numRes, _) = runMath (sub2 a b)
          numResInt = convertFrom numRes
      in intRes == numResInt

-- -----------------------------------------------------------------------------
--
-- Finally, multiplication and Karatsuba
--
-- -----------------------------------------------------------------------------

mul1 :: Number -> Number -> Math Number
mul1 num1 num2
  | size num1 /= 1 || size num2 /= 1 =
      fail "Called mul1 with !1 digit numbers. Idiot."
  | otherwise =
      do x'   <- embiggen (V.head num1)
         y'   <- embiggen (V.head num2)
         comb <- x' |*| y'
         z0   <- bottomBits comb
         z1   <- bottomBits =<< (comb |>>| 64)
         return (V.fromList [z0, z1])

prop_MulNWorks :: Int -> (Number -> Number -> Math Number) ->
                  Number -> Number ->
                  Bool
prop_MulNWorks nsize f x y =
  let (x', _) = runMath (padTo nsize (V.take nsize x))
      (y', _) = runMath (padTo nsize (V.take nsize y))
      xInt = convertFrom x'
      yInt = convertFrom y'
      resInt = xInt * yInt
      (resNum, _) = runMath (f x' y')
  in (size x' == nsize) && (size y' == nsize) &&
     (size resNum == (nsize * 2)) &&
     (resInt == convertFrom resNum)

prop_Mul1Works :: Number -> Number -> Bool
prop_Mul1Works = prop_MulNWorks 1 mul1

mul2 :: Number -> Number -> Math Number
mul2 num1 num2
  | size num1 /= 2 || size num2 /= 2 =
      fail "Called mul2 with !2 digit numbers. Idiot."
  | otherwise =
      do [l0, l1] <- mapM embiggen (V.toList num1)
         [r0, r1] <- mapM embiggen (V.toList num2)
         --
         l0r0     <- l0 |*| r0
         carry0   <- l0r0 |>>| 64
         dest0    <- bottomBits l0r0
         l1r0     <- l1 |*| r0
         l1r0'    <- l1r0 |+| carry0
         tdest1   <- l1r0' |&| 0xFFFFFFFFFFFFFFFF
         tdest2   <- l1r0' |>>| 64
         --
         l0r1     <- l0 |*| r1
         l0r1'    <- tdest1 |+| l0r1
         dest1    <- bottomBits l0r1'
         l1r1     <- l1 |*| r1
         l1r1'    <- tdest2 |+| l1r1
         carry1   <- l0r1' |>>| 64
         l1r1''   <- l1r1' |+| carry1
         dest2    <- bottomBits l1r1''
         dest3    <- bottomBits =<< (l1r1'' |>>| 64)
         return (V.fromList [dest0, dest1, dest2, dest3])

prop_Mul2Works :: Number -> Number -> Bool
prop_Mul2Works = prop_MulNWorks 2 mul2

mul3 :: Number -> Number -> Math Number
mul3 num1 num2
  | size num1 /= 3 || size num2 /= 3 =
      fail "Called mul2 with !2 digit numbers. Idiot."
  | otherwise =
      do [l0, l1, l2] <- mapM embiggen (V.toList num1)
         [r0, r1, r2] <- mapM embiggen (V.toList num2)
         --
         l0r0   <- l0 |*| r0
         dest0  <- bottomBits l0r0
         carry0 <- l0r0 |>>| 64
         l1r0   <- l1 |*| r0
         l1r0'  <- l1r0 |+| carry0
         tdest1 <- l1r0' |&| 0xFFFFFFFFFFFFFFFF
         carry1 <- l1r0' |>>| 64
         l2r0   <- l2 |*| r0
         l2r0'  <- l2r0 |+| carry1
         tdest2 <- l2r0' |&| 0xFFFFFFFFFFFFFFFF
         tdest3 <- l2r0' |>>| 64
         --
         l0r1    <- l0 |*| r1
         l0r1'   <- tdest1 |+| l0r1
         dest1   <- bottomBits l0r1'
         carry2  <- l0r1' |>>| 64
         l1r1    <- l1 |*| r1
         l1r1'   <- sumDigits [l1r1, tdest2, carry2]
         tdest2' <- l1r1' |&| 0xFFFFFFFFFFFFFFFF
         carry3  <- l1r1' |>>| 64
         l2r1    <- l2 |*| r1
         l2r1'   <- sumDigits [l2r1, tdest3, carry3]
         tdest3' <- l2r1' |&| 0xFFFFFFFFFFFFFFFF
         tdest4' <- l2r1' |>>| 64
         --
         l0r2    <- l0 |*| r2
         l0r2'   <- l0r2 |+| tdest2'
         dest2   <- bottomBits l0r2'
         carry4  <- l0r2' |>>| 64
         l1r2    <- l1 |*| r2
         l1r2'   <- sumDigits [l1r2, tdest3', carry4]
         dest3   <- bottomBits l1r2'
         carry5  <- l1r2' |>>| 64
         l2r2    <- l2 |*| r2
         l2r2'   <- sumDigits [l2r2, tdest4', carry5]
         dest4   <- bottomBits l2r2'
         dest5   <- bottomBits =<< (l2r2' |>>| 64)
         return (V.fromList [dest0, dest1, dest2, dest3, dest4, dest5])

prop_Mul3Works :: Number -> Number -> Bool
prop_Mul3Works = prop_MulNWorks 3 mul3

karatsuba :: Number -> Number -> Math Number
karatsuba num1 num2
  | size num1 /= size num2 =
      fail "Uneven numeric lengths!"
  | empty num1 =
      fail "Got empty nums"
  | size num1 == 1 = mul1 num1 num2
  | size num1 == 2 = mul2 num1 num2
  | size num1 == 3 = mul3 num1 num2
  | otherwise =
      do let m  = min (size num1) (size num2)
             m2 = m `div` 2
         (low1, high1) <- splitDigits (fromIntegral m2) num1
         (low2, high2) <- splitDigits (fromIntegral m2) num2
         z0            <- karatsuba low1 low2
         let midsize = max (size low1) (size high1)
         low1'         <- padTo midsize low1
         low2'         <- padTo midsize low2
         high1'        <- padTo midsize high1
         high2'        <- padTo midsize high2
         mid1          <- add2 low1' high1'
         mid2          <- add2 low2' high2'
         z1            <- karatsuba mid1 mid2
         z2            <- karatsuba high1 high2
         let subsize = max (size z0) (max (size z1) (size z2))
         sz0           <- padTo subsize z0
         sz1           <- padTo subsize z1
         sz2           <- padTo subsize z2
         tmp           <- sub2 sz1 sz2
         z1'           <- addZeros m2 =<< sub2 tmp sz0
         z2'           <- addZeros (m2 * 2) z2
         let addsize = max (size z0) (max (size z1') (size z2'))
         az0           <- padTo addsize z0
         az1           <- padTo addsize z1'
         az2           <- padTo addsize z2'
         add3 az2 az1 az0

prop_KaratsubaWorks :: Number -> Number -> Bool
prop_KaratsubaWorks x y =
  let shouldBe = convertFrom x * convertFrom y
      monad    = karatsuba x y
      myVersion = convertFrom (fst (runMath monad))
  in shouldBe == myVersion

prop_InstructionsWork :: Number -> Number -> Bool
prop_InstructionsWork x y =
  let shouldBe       = convertFrom x' * convertFrom y'
      (mine, instrs) = runMath (karatsuba x' y')
      myVersion      = convertFrom mine
      (endEnv, _)    = run startEnv instrs
      instrVersion   = V.map (getv endEnv . name) mine
  in (shouldBe == myVersion) && (mine == instrVersion)
 where
  x' = rename "x" x
  y' = rename "y" y
  startEnv = (Map.fromList startEnv64, Map.empty)
  startEnv64 = map (\ d -> (name d, digit d)) (V.toList (x' <> y'))
  getv env n =
    case Map.lookup n env of
      Nothing -> error ("InstrProp lookup failure: " ++ n)
      Just v  -> D n v

prop_InstructionsConsistent :: Number -> Number -> Number -> Number -> Bool
prop_InstructionsConsistent a b x y =
  let (_, instrs1) = runMath (karatsuba a' b')
      (_, instrs2) = runMath (karatsuba x' y')
  in instrs1 == instrs2
 where
  a' = rename "p" a
  b' = rename "q" b
  x' = rename "p" x
  y' = rename "q" y

-- -----------------------------------------------------------------------------
--
-- Test running
--
-- -----------------------------------------------------------------------------

runQuickCheck :: Testable prop => String -> prop -> IO ()
runQuickCheck testname prop =
  do putStr testname
     quickCheck (withMaxSuccess 1000 prop)

runChecks :: IO ()
runChecks =
  do runQuickCheck "Num -> Int -> Num     " prop_ConversionWorksNum
     runQuickCheck "Int -> Num -> Int     " prop_ConversionWorksInt
     runQuickCheck "Split Isn't Dumb      " prop_SplitDigitsIsntTerrible
     runQuickCheck "More 0s is Shift      " prop_AddZerosIsShift
     runQuickCheck "PadTo Does That       " prop_PadToWorks
     runQuickCheck "Add2 Works            " prop_Add2Works
     runQuickCheck "Add3 Works            " prop_Add3Works
     runQuickCheck "Sub2 Works            " prop_Sub2Works
     runQuickCheck "Mul1 Works            " prop_Mul1Works
     runQuickCheck "Mul2 Works            " prop_Mul2Works
     runQuickCheck "Mul3 Works            " prop_Mul3Works
     runQuickCheck "Karatsuba Works       " prop_KaratsubaWorks
     runQuickCheck "Instructions Work     " prop_InstructionsWork
     runQuickCheck "Generation Consistent " prop_InstructionsConsistent
