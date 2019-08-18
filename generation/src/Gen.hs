{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Gen(
    Gen(Gen),
    runGen,
    gensym,
    indent,
    blank,
    out,
    wrapIndent,
    implFor,
    implFor',
    implFor'',
    newNum,
    TestVariable(..),
    )
 where

import Control.Monad.RWS.Strict(RWS,evalRWS)
import Control.Monad.State.Class(MonadState,get,put)
import Control.Monad.Writer.Class(MonadWriter,tell)
import Data.Bits(shiftL)
import Data.List(replicate)
import Data.Word(Word)
import Numeric(showHex)
import System.Random(StdGen, newStdGen, random, randomR)

newtype Gen a = Gen { unGen :: RWS () String GenState a}
 deriving (Applicative, Functor, Monad, MonadState GenState, MonadWriter String)

tabAmount :: Word
tabAmount = 4

data GenState = GenState {
    indentAmount :: Word,
    gensymIndex  :: Word,
    rng          :: StdGen
}

initGenState :: IO GenState
initGenState =
    do rng0 <- newStdGen
       return GenState { indentAmount = 0, gensymIndex = 0, rng = rng0 }

runGen :: FilePath -> Gen a -> IO a
runGen path action =
    do state0 <- initGenState
       let (res, contents) = evalRWS (unGen action) () state0
       writeFile path contents
       return res

gensym :: String -> Gen String
gensym prefix =
    do gs <- get
       let gs' = gs{ gensymIndex = gensymIndex gs + 1 }
       put gs'
       return (prefix ++ show (gensymIndex gs))

indent :: Gen a -> Gen a
indent action =
    do gs <- get
       put gs{ indentAmount = indentAmount gs + tabAmount }
       res <- action
       put gs
       return res

blank :: Gen ()
blank = tell "\n"

out :: String -> Gen ()
out val =
    do gs <- get
       tell (replicate (fromIntegral (indentAmount gs)) ' ')
       tell val
       tell "\n"

wrapIndent :: String -> Gen a -> Gen a
wrapIndent val middle =
    do gs <- get
       tell (replicate (fromIntegral (indentAmount gs)) ' ')
       tell val
       tell " {\n"
       res <- indent middle
       tell (replicate (fromIntegral (indentAmount gs)) ' ')
       tell "}\n"
       return res

implFor :: String -> String -> Gen a -> Gen a
implFor trait name middle =
    wrapIndent ("impl " ++ trait ++ " for " ++ name) middle

implFor' :: String -> String -> Gen a -> Gen a
implFor' trait name middle =
    wrapIndent ("impl<'a> " ++ trait ++ " for " ++ name) middle

implFor'' :: String -> String -> Gen a -> Gen a
implFor'' trait name middle =
    wrapIndent ("impl<'a,'b> " ++ trait ++ " for " ++ name) middle

newNum :: Bool -> Word -> Gen Integer
newNum signed bits =
    do gs <- get
       let rng0 = rng gs
       let high = (1 `shiftL` fromIntegral bits) - 1
       let (v, rng1) = randomR (0, high) rng0
       let (sign, rng2) = random rng1
       let v' = if signed && sign then -v else v
       put gs{ rng = rng2 }
       return v'

class TestVariable a where
    emitTestVariable :: Char -> a -> Gen ()

instance TestVariable Integer where
    emitTestVariable c v =
        out ([c] ++ ": " ++ showHex v "")