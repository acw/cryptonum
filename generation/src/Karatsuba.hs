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
import Debug.Trace
import Prelude hiding (fail)
import Test.QuickCheck hiding ((.&.))

-- this drives the testing
inputWordSize :: Int
inputWordSize = 5

generateInstructions :: Word -> [Instruction]
generateInstructions numdigits =
  let (baseVec, baseInstrs) = runMath $ do x <- V.replicateM (fromIntegral numdigits) (genDigit 1)
                                           y <- V.replicateM (fromIntegral numdigits) (genDigit 1)
                                           karatsuba x y
  in baseInstrs

--   foldl replaceVar baseInstrs varRenames
-- where
--  x = rename "x" (V.replicate (fromIntegral numdigits) (D "" 1))
--  y = rename "y" (V.replicate (fromIntegral numdigits) (D "" 1))
--  (baseVec, baseInstrs) = runMath (karatsuba x y)
--  res = rename "res" baseVec
--  varRenames = zip (map name (V.toList baseVec)) (map name (V.toList res))

-- -----------------------------------------------------------------------------
--
-- Instructions that we emit as a result of running Karatsuba, that can be
-- turned into Rust lines.
--
-- -----------------------------------------------------------------------------

newtype Variable = V String
 deriving (Eq, Ord, Show)

-- these are in Intel form, as I was corrupted young, so the first argument
-- is the destination and the rest are the arguments.
data Instruction = Add        Variable [Variable]
                 | CastDown   Variable Variable
                 | CastUp     Variable Variable
                 | Complement Variable Variable
                 | Declare64  Variable Word64
                 | Declare128 Variable Word128
                 | Mask       Variable Variable   Word128
                 | Multiply   Variable [Variable]
                 | ShiftR     Variable Variable   Int
 deriving (Eq, Show)

class Declarable a where
  declare :: Variable -> a -> Instruction

instance Declarable Word64 where
  declare n x = Declare64  n x
instance Declarable Word128 where
  declare n x = Declare128 n x

type Env = (Map Variable Word64, Map Variable Word128)

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
  getv :: Map Variable a -> Variable -> a
  getv env s =
    case Map.lookup s env of
      Nothing -> error ("Failure to find key '" ++ show s ++ "'")
      Just v -> v

run :: Env -> [Instruction] -> Env
run env instrs =
  case instrs of
    []       -> env
    (x:rest) -> run (step env x) rest

replaceVar :: [Instruction] -> (Variable, Variable) -> [Instruction]
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

newVariable :: Math Variable
newVariable =
  do x <- state (\ i -> (i, i + 1))
     return (V (show x))

runMath :: Math a -> (a, [Instruction])
runMath m = evalRWS (unMath m) () 0

-- -----------------------------------------------------------------------------
--
-- Primitive mathematics that can run on a Digit
--
-- -----------------------------------------------------------------------------

data Digit size = D  {
   name     :: Variable
 , digit    :: size
 }
 deriving (Eq,Show)

genDigit :: Declarable size => size -> Math (Digit size)
genDigit x =
  do newName <- newVariable
     emit (declare newName x)
     return D{
       name  = newName
     , digit = x
     }

embiggen :: Digit Word64 -> Math (Digit Word128)
embiggen x =
  do newName <- newVariable
     emit (CastUp newName (name x))
     return (D newName (fromIntegral (digit x)))

bottomBits :: Digit Word128 -> Math (Digit Word64)
bottomBits x =
  do newName <- newVariable
     emit (CastDown newName (name x))
     return (D newName (fromIntegral (digit x)))

oneDigit :: Math (Digit Word64)
oneDigit = genDigit 1

bigZero :: Math (Digit Word128)
bigZero = genDigit 0

(|+|) :: Digit Word128 -> Digit Word128 -> Math (Digit Word128)
(|+|) x y =
  do newName <- newVariable
     emit (Add newName [name x, name y])
     let digval = digit x + digit y
     return (D newName digval)

sumDigits :: [Digit Word128] -> Math (Digit Word128)
sumDigits ls =
  do newName <- newVariable
     emit (Add newName (map name ls))
     let digval = sum (map digit ls)
     return (D newName digval)

(|*|) :: Digit Word128 -> Digit Word128 -> Math (Digit Word128)
(|*|) x y =
  do newName <- newVariable
     emit (Multiply newName [name x, name y])
     let digval = digit x * digit y
     return (D newName digval)

(|>>|) :: Digit Word128 -> Int -> Math (Digit Word128)
(|>>|) x s =
  do newName <- newVariable
     emit (ShiftR newName (name x) s)
     let digval = digit x `shiftR` s
     return (D newName digval)

(|&|) :: Digit Word128 -> Word128 -> Math (Digit Word128)
(|&|) x m =
  do newName <- newVariable
     emit (Mask newName (name x) m)
     let digval = digit x .&. m
     return (D newName digval)

complementDigit :: Digit Word64 -> Math (Digit Word64)
complementDigit x =
  do newName <- newVariable
     emit (Complement newName (name x))
     return (D newName (complement (digit x)))

-- -----------------------------------------------------------------------------
--
-- Extended mathematics that run on whole numbers
--
-- -----------------------------------------------------------------------------

type Number = Vector (Digit Word64)

convertTo :: Int -> Integer -> Math Number
convertTo sz num = V.fromList `fmap` go sz num
 where
  go :: Int -> Integer -> Math [Digit Word64]
  go 0 _ =
    return []
  go x v =
    do d <- genDigit (fromIntegral v)
       rest <- go (x - 1) (v `shiftR` 64)
       return (d:rest)

convertFrom :: Number -> Integer
convertFrom n = V.foldr combine 0 n
 where
  combine x acc = (acc `shiftL` 64) + fromIntegral (digit x)

prop_ConversionWorksInt :: Integer -> Bool
prop_ConversionWorksInt n = n' == back
 where
  n' = abs n `mod` (2 ^ (inputWordSize * 64))
  there = fst (runMath (convertTo inputWordSize n'))
  back = convertFrom there

zero :: Int -> Math Number
zero s = V.fromList `fmap` replicateM s (genDigit 0)

empty :: Number -> Bool
empty = null

size :: Number -> Int
size = length

splitDigits :: Int -> Number -> Math (Number, Number)
splitDigits i ls = return (V.splitAt i ls)

prop_SplitDigitsIsntTerrible :: Int -> Int -> Integer -> Bool
prop_SplitDigitsIsntTerrible a b n =
  let a' = a `mod` 20
      b' = b `mod` 20
      (p, l) | a' >  b'  = (b', a')
             | a' <  b'  = (a', b')
             | otherwise = (a' - 1, a')
  in fst $ runMath $ do base <- convertTo l n
                        (left, right) <- splitDigits p base
                        return (base == (left <> right))

addZeros :: Int -> Number -> Math Number
addZeros x n =
  do prefix <- zero x
     return (prefix <> n)

prop_AddZerosIsShift :: Int -> Integer -> Bool
prop_AddZerosIsShift x n =
  fst $ runMath $ do base <- convertTo inputWordSize n'
                     added <- addZeros x' base
                     let shiftVer = n' `shiftL` (x' * 64)
                     let mine = convertFrom added
                     return (shiftVer == mine) 
 where
  x' = abs x `mod` inputWordSize
  n' = abs n `mod` (2 ^ (inputWordSize * 64))
                     
padTo :: Int -> Number -> Math Number
padTo len num =
  do suffix <- zero (len - V.length num)
     return (num <> suffix)

prop_PadToWorks :: Int -> Int -> Integer -> Bool
prop_PadToWorks a b num =
  fst $ runMath $ do base <- convertTo sz num'
                     padded <- padTo len base
                     let newval = convertFrom padded
                     return (num' == newval)
 where
  a' = abs a `mod` (inputWordSize * 3)
  b' = abs b `mod` (inputWordSize * 3)
  (len, sz) | a' >= b'  = (max 1 a', max 1 b')
            | otherwise = (max 1 b', max 1 a')
  num' = abs (num `mod` (2 ^ (64 * sz)))

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

prop_Add2Works :: Int -> Integer -> Integer -> Bool
prop_Add2Works l n m =
  fst $ runMath $ do num1 <- convertTo l' n'
                     num2 <- convertTo l' m'
                     res  <- add2 num1 num2
                     let intRes = convertFrom res
                     return ((intRes == r) && (size res == l' + 1))
 where
  l' = max 1 (abs l `mod` inputWordSize)
  n' = abs n `mod` (2 ^ (l' * 64))
  m' = abs m `mod` (2 ^ (l' * 64))
  r  = n' + m'

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

prop_Add3Works :: Int -> Integer -> Integer -> Integer -> Bool
prop_Add3Works l x y z =
  fst $ runMath $ do num1 <- convertTo l' x'
                     num2 <- convertTo l' y'
                     num3 <- convertTo l' z'
                     res  <- add3 num1 num2 num3
                     let intRes = convertFrom res
                     return ((intRes == r) && (size res == l' + 1))
 where
  l' = max 1 (abs l `mod` inputWordSize)
  x' = abs x `mod` (2 ^ (l' * 64))
  y' = abs y `mod` (2 ^ (l' * 64))
  z' = abs z `mod` (2 ^ (l' * 64))
  r  = x' + y' + z'

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

prop_Sub2Works :: Int -> Integer -> Integer -> Bool
prop_Sub2Works l a b =
  fst $ runMath $ do num1 <- convertTo l' x
                     num2 <- convertTo l' y
                     res  <- sub2 num1 num2
                     let intRes = convertFrom res
                     return (intRes == r)
 where
  l' = max 1 (abs l `mod` inputWordSize)
  a' = abs a `mod` (2 ^ (l' * 64))
  b' = abs b `mod` (2 ^ (l' * 64))
  (x, y) | a' >= b'  = (a', b')
         | otherwise = (b', a')
  r = x - y

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
                  Integer -> Integer ->
                  Bool
prop_MulNWorks nsize f x y =
  fst $ runMath $ do num1 <- convertTo nsize x'
                     num2 <- convertTo nsize y'
                     res  <- f num1 num2
                     let resInt = convertFrom res
                     return ((size res == (nsize * 2)) && (resInt == (x' * y')))
                    
 where
  x' = abs x `mod` (2 ^ (64 * nsize))
  y' = abs y `mod` (2 ^ (64 * nsize))

prop_Mul1Works :: Integer -> Integer -> Bool
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

prop_Mul2Works :: Integer -> Integer -> Bool
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

prop_Mul3Works :: Integer -> Integer -> Bool
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
         res           <- add3 az2 az1 az0
         forM_ (V.drop (m * 2) res) $ \ highDigit ->
           -- this will only occur when (size res > (m * 2))
           when (digit highDigit /= 0) $
             fail "High bit found in Karatsuba result"
         return (V.take (m * 2) res)

prop_KaratsubaWorks :: Int -> Integer -> Integer -> Bool
prop_KaratsubaWorks l x y =
  fst $ runMath $ do num1 <- convertTo l' x'
                     num2 <- convertTo l' y'
                     res  <- karatsuba num1 num2
                     let resInt = convertFrom res
                         sizeOk = size res == (l' * 2)
                         valOk = resInt == (x' * y')
                     return (sizeOk && valOk)
                    
 where
  l' = (abs l `mod` (inputWordSize * 2)) + 2
  x' = abs x `mod` (2 ^ (64 * l'))
  y' = abs y `mod` (2 ^ (64 * l'))


prop_InstructionsWork :: Int -> Integer -> Integer -> Bool
prop_InstructionsWork l x y =
  let (value, instructions) = runMath $ do numx <- convertTo l' x'
                                           numy <- convertTo l' y'
                                           karatsuba numx numy
      resGMP                = x' * y'
      resKaratsuba          = convertFrom value
      (endEnvironment, _)   = run (Map.empty, Map.empty) instructions
      instrVersion          = V.map (getv endEnvironment . name) value
  in (resGMP == resKaratsuba) && (value == instrVersion)
 where
  l' = max 1 (abs l `mod` inputWordSize)
  x' = abs x `mod` (2 ^ (64 * l'))
  y' = abs y `mod` (2 ^ (64 * l'))
  getv env n =
    case Map.lookup n env of
      Nothing -> error ("InstrProp lookup failure: " ++ show n)
      Just v  -> D n v

prop_InstructionsConsistent :: Int -> Integer -> Integer -> Integer -> Integer -> Bool
prop_InstructionsConsistent l a b x y =
  let (_, instrs1) = runMath (karatsuba' a' b')
      (_, instrs2) = runMath (karatsuba' x' y')
      instrs1' = dropWhile isDeclare64 instrs1
      instrs2' = dropWhile isDeclare64 instrs2
  in instrs1' == instrs2'
 where
  l' = max 1 (abs l `mod` inputWordSize)
  a' = abs a `mod` (2 ^ (64 * l'))
  b' = abs b `mod` (2 ^ (64 * l'))
  x' = abs x `mod` (2 ^ (64 * l'))
  y' = abs y `mod` (2 ^ (64 * l'))
  karatsuba' p q =
    do num1 <- convertTo l' p
       num2 <- convertTo l' q
       karatsuba num1 num2
  isDeclare64 i =
    case i of
      Declare64 _ _ -> True
      _             -> False

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
  do runQuickCheck "Int -> Num -> Int     " prop_ConversionWorksInt
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
