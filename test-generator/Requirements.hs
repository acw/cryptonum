module Requirements(
         Operation(..),
         Requirement(..),
         requirements
       )
 where

import Data.List(sort)

data Operation = Add
               | BaseOps
               | Barretts
               | Div
               | ModExp
               | ModMul
               | ModSq
               | Mul
               | Shifts
               | Square
               | Sub
               | Convert Int
               | SignedAdd
               | SignedBase
               | SignedCmp
               | SignedShift
               | SignedSub
               | SigConvert Int
               | EGCD
               | ModInv
               | PrimeGen
               | RSA
               | DSA
 deriving (Eq, Ord, Show)

data Requirement = Req Int Operation
 deriving (Eq, Ord, Show)

data Need = Need Operation (Int -> [Requirement])

needs :: [Need]
needs = [ Need RSA         (\ size -> [Req (size `div` 2) Sub,
                                       Req (size `div` 2) Mul,
                                       Req (size `div` 2) PrimeGen,
                                       Req size           BaseOps,
                                       Req size           ModInv,
                                       Req size           ModExp
                                      ])
        , Need DSA         (\ size -> [Req size BaseOps,
                                       Req size Shifts,
                                       Req size Add])
        , Need PrimeGen    (\ size -> [Req size Div,
                                       Req size Shifts,
                                       Req size ModExp,
                                       Req size EGCD])
        , Need Add         (\ size -> [Req size BaseOps,
                                       Req (size + 64) BaseOps,
                                       Req size (Convert (size + 64))
                                      ])
        , Need Barretts    (\ size -> [Req size BaseOps,
                                       Req (size + 64) BaseOps,
                                       Req (size * 2) BaseOps,
                                       Req ((size * 2) + 64) BaseOps,
                                       Req size (Convert ((size * 2) + 64)),
                                       Req (size + 64) Mul,
                                       Req ((size * 2) + 64) Add,
                                       Req ((size * 2) + 64) Sub,
                                       Req (size + 64) (Convert ((size * 2) + 64)),
                                       Req ((size * 2) + 64) (Convert ((size + 64) * 2)),
                                       Req (size * 2) (Convert ((size * 2) + 64)),
                                       Req (size + 64) (Convert ((size + 64) * 2)),
                                       Req (size + 64) (Convert (size * 2)),
                                       Req (size * 2) Shifts,
                                       Req ((size + 64) * 2) Shifts,
                                       Req ((size * 2) + 64) Div
                                      ])
        , Need Div         (\ size -> [Req size BaseOps,
                                       Req (size * 2) BaseOps,
                                       Req size (Convert (size * 2)),
                                       Req (size * 2) Sub,
                                       Req size Mul,
                                       Req 192 BaseOps,
                                       Req 192 Mul,
                                       Req 384 BaseOps
                                      ])
        , Need ModExp      (\ size -> [Req size BaseOps,
                                       Req size Barretts,
                                       Req size ModSq,
                                       Req size ModMul,
                                       Req size (Convert (size + 64))
                                      ])
        , Need ModMul      (\ size -> [Req size BaseOps,
                                       Req (size * 2) BaseOps,
                                       Req size Barretts,
                                       Req size Mul,
                                       Req size (Convert (size + 64))
                                      ])
        , Need ModSq       (\ size -> [Req size BaseOps,
                                       Req (size * 2) BaseOps,
                                       Req size Barretts,
                                       Req size Square,
                                       Req (size * 2) Div,
                                       Req size (Convert (size * 2)),
                                       Req size (Convert (size + 64))
                                      ])
        , Need Mul         (\ size -> [Req size BaseOps,
                                       Req (size * 2) BaseOps,
                                       Req size (Convert (size * 2))
                                      ])
        , Need Shifts      (\ size -> [Req size BaseOps
                                      ])
        , Need Square      (\ size -> [Req size BaseOps,
                                       Req (size * 2) BaseOps
                                      ])
        , Need Sub         (\ size -> [Req size BaseOps
                                      ])
        , Need SignedAdd   (\ size -> [Req size SignedBase,
                                       Req (size + 64) SignedBase,
                                       Req (size + 64) BaseOps
                                      ])
        , Need SignedBase  (\ size -> [Req size BaseOps])
        , Need SignedCmp   (\ size -> [Req size BaseOps])
        , Need SignedShift (\ size -> [Req size SignedBase,
                                       Req size BaseOps,
                                       Req size Shifts,
                                       Req size Add
                                      ])
        , Need SignedSub   (\ size -> [Req size SignedBase,
                                       Req (size + 64) SignedBase,
                                       Req (size + 64) BaseOps,
                                       Req size Add,
                                       Req size Sub,
                                       Req size (Convert (size + 64)),
                                       Req size (SigConvert (size + 64))
                                      ])
        , Need EGCD        (\ size -> [Req size SignedBase,
                                       Req size BaseOps,
                                       Req (size + 64) SignedBase,
                                       Req size (SigConvert (size + 64)),
                                       Req (size + 64) SignedShift,
                                       Req (size + 64) SignedAdd,
                                       Req (size + 64) SignedSub,
                                       Req (size + 64) SignedCmp
                                      ])
        , Need ModInv      (\ size -> [Req size BaseOps,
                                       Req (size + 64) SignedBase,
                                       Req (size + 64) BaseOps,
                                       Req size (Convert (size + 64)),
                                       Req size EGCD,
                                       Req (size + 64) SignedAdd,
                                       Req size Barretts
                                      ])
        ]

newRequirements :: Requirement -> [Requirement]
newRequirements (Req size op) = concatMap go needs ++ [Req size BaseOps]
 where
  go (Need op2 generator) | op == op2 = generator size
                          | otherwise = []

rsaSizes :: [Int]
rsaSizes =  [512,1024,2048,3072,4096,8192,15360]

dsaSizes :: [Int]
dsaSizes =  [192,256,1024,2048,3072]

baseRequirements :: [Requirement]
baseRequirements = concatMap (\ x -> [Req x RSA]) rsaSizes
                ++ concatMap (\ x -> [Req x DSA]) dsaSizes
                ++ [Req 192 (Convert 1024), Req 256 (Convert 2048), Req 256 (Convert 3072)] -- used in DSA
                ++ [Req 192 Add, Req 256 Add, Req 384 Add] -- used for testing
                ++ [Req 192 Mul, Req 384 Mul] -- used for testing
                ++ [Req 448 (Convert 512)] -- used for testing

requirements :: [Requirement]
requirements = go baseRequirements
 where
  step ls = let news = concatMap newRequirements ls
                destBits = concatMap destRequirements (news ++ ls)
            in ls ++ news ++ destBits
  --
  go ls = let ls' = removeDups (sort (step ls))
          in if ls == ls' then ls else go ls'
  --
  removeDups [] = []
  removeDups (x:xs) | x `elem` xs = removeDups xs
                    | otherwise   = x : removeDups xs
  --
  destRequirements (Req _ (Convert t)) = [Req t BaseOps]
  destRequirements _                   = []

