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
               | SignedMul
               | SignedDiv
               | SigConvert Int
               | SquareRoot
               | EGCD
               | ModInv
               | PrimeGen
               | RSA
               | DSA
               | ECDSA
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
                                       Req size Add,
                                       Req size SquareRoot,
                                       Req size PrimeGen,
                                       Req size ModInv,
                                       Req size Mul,
                                       Req (size * 2) Add,
                                       Req (((size * 2) + 64) * 2) Div,
                                       Req size (Convert 512),
                                       Req size (Convert (size + 128)),
                                       Req size (Convert ((size * 2) + 64)),
                                       Req size (Convert (((size * 2) + 64) * 2))
                                       ])
        , Need ECDSA       (\ size -> [Req size SignedSub,
                                       Req (size + 64) SignedMul,
                                       Req ((size + 64) * 2) SignedSub,
                                       Req ((size + 64) * 2) SignedDiv,
                                       Req size (Convert ((size + 64) * 2)),
                                       Req size (SigConvert ((size + 64) * 2))
                                       ])
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
                                       Req (size + 64) Sub,
                                       Req size (Convert (size + 64)),
                                       Req size (SigConvert (size + 64))
                                      ])
        , Need SignedMul   (\ size -> [Req size Mul,
                                       Req (size * 2) SignedBase,
                                       Req size (SigConvert (size * 2))
                                      ])
        , Need SignedDiv   (\ size -> [Req size Div
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
        , Need SquareRoot  (\ size -> [Req size BaseOps,
                                       Req size Shifts,
                                       Req size Add,
                                       Req size Sub
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

ecdsaSizes :: [Int]
ecdsaSizes = [192,256,384,576]

baseRequirements :: [Requirement]
baseRequirements = concatMap (\ x -> [Req x RSA]) rsaSizes
                ++ concatMap (\ x -> [Req x DSA]) dsaSizes
                ++ concatMap (\ x -> [Req x ECDSA]) ecdsaSizes
                ++ [Req 192 (Convert 1024), Req 256 (Convert 2048), Req 256 (Convert 3072)] -- used in DSA
                ++ [Req 384 (Convert 1024), Req 512 (Convert 2048), Req 512 (Convert 3072)] -- used in DSA
                ++ [Req 192 Add, Req 256 Add, Req 384 Add] -- used for testing
                ++ [Req 192 Mul, Req 384 Mul] -- used for testing
                ++ [Req 448 (Convert 512)] -- used for testing

requirements :: [Requirement]
requirements = go baseRequirements
 where
  step ls = let news  = concatMap newRequirements ls
                ls'   = concatMap sanitizeConverts (news ++ ls)
                ls''  = removeDups (sort ls')
            in ls''
  --
  go ls = let ls' = step ls
          in if ls == ls' then ls else go ls'
  --
  removeDups [] = []
  removeDups (x:xs) | x `elem` xs = removeDups xs
                    | otherwise   = x : removeDups xs
  --
  sanitizeConverts (Req x (Convert y))
    | x == y    = []
    | x < y     = [Req x (Convert y), Req y BaseOps]
    | otherwise = [Req y (Convert x), Req x BaseOps]
  sanitizeConverts x = [x]
