{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Gen(
    toLit
    )
 where

import Language.Rust.Data.Position
import Language.Rust.Syntax

toLit :: Word -> Expr Span
toLit i = Lit  [] (Int Dec (fromIntegral i) Unsuffixed mempty) mempty


