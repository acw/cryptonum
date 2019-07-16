{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Gen(
    Gen(Gen),
    runGen,
    gensym,
    indent,
    blank,
    out,
    wrapIndent,
    )
 where

import Control.Monad.RWS.Strict(RWS,evalRWS)
import Control.Monad.State.Class(MonadState,get,put)
import Control.Monad.Writer.Class(MonadWriter,tell)
import Data.List(replicate)
import Data.Word(Word)

newtype Gen a = Gen { unGen :: RWS () String GenState a}
 deriving (Applicative, Functor, Monad, MonadState GenState, MonadWriter String)

tabAmount :: Word
tabAmount = 4

data GenState = GenState {
    indentAmount :: Word,
    gensymIndex  :: Word
}

initGenState :: GenState
initGenState =  GenState { indentAmount = 0, gensymIndex = 0 }

runGen :: FilePath -> Gen a -> IO a
runGen path action =
    do let (res, contents) = evalRWS (unGen action) () initGenState
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