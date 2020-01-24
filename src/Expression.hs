{-# LANGUAGE TypeOperators #-}

module Expression
  ( POLE (..),
    Expression (..),
    Inference (..),
    prettyExpr,
    prettyPrint) where

data POLE = LEFT | RIGHT | NEITHER  
  deriving
    ( Show
    , Eq)

data Expression
  = Atom String 
  | TRUE 
  | FALSE
  | And Expression Expression
  | Or  Expression Expression
  | Imp Expression Expression
  deriving
    ( Read
    , Show
    , Eq)
    --Functor,Foldable,Traversable)

infixr `Imp`

data Inference = Inference {
  delta :: [Expression],
  omega :: [Expression],
  pole :: POLE, 
  express :: Expression
} deriving
    (Show
    , Eq)

prettyPole :: POLE -> String 
prettyPole LEFT = "L "
prettyPole RIGHT = "R "
prettyPole NEITHER = " "  

prettyExpr :: Expression -> String
prettyExpr (Atom s) = s
prettyExpr TRUE = "T"
prettyExpr FALSE = "F"
prettyExpr (And left right) = "(" ++ prettyExpr left ++ " & " ++ prettyExpr right ++ ")"
prettyExpr (Or  left right) = "(" ++ prettyExpr left ++ " v " ++ prettyExpr right ++ ")"
prettyExpr (Imp left right) = "(" ++ prettyExpr left ++ " -> " ++ prettyExpr right ++ ")"

prettyPrint :: Inference -> String 
prettyPrint Inference {delta = delt, omega = omeg, pole = lrc, express = expr} = 
  show (fmap prettyExpr delt) 
  ++ "," 
  ++ show (fmap prettyExpr omeg)
  ++ " ===>>" 
  ++ prettyPole lrc
  ++ show(prettyExpr expr)
