{-# LANGUAGE TypeOperators #-}

module Expression
  ( POLE (..),
    Expression (..),
    (&),
    (\/),
    (-->),
    (<->),
    neg,
    Inference (..)) where

data POLE = LEFT | RIGHT | NEITHER  
  deriving
    (Eq)

instance Show POLE where
  show LEFT = "L "
  show RIGHT = "R "
  show NEITHER = " "  


data Expression
  = Atom String 
  | TRUE 
  | FALSE
  | And Expression Expression
  | Or  Expression Expression
  | Imp Expression Expression
  deriving
    ( Read
    , Eq)
    --Functor,Foldable,Traversable)

infixr 3 &
infixr 2 \/
infixr 1 -->
infixr 0 <->

(&) :: Expression -> Expression -> Expression
(&) = And

(\/) :: Expression -> Expression -> Expression
(\/) = Or

(-->) :: Expression -> Expression -> Expression
(-->) = Imp

(<->) :: Expression -> Expression -> Expression
a <-> b = And (Imp a b) (Imp b a)

neg :: Expression -> Expression
neg a = a --> FALSE

instance Show Expression where
  show (Atom s) = s
  show TRUE = "T"
  show FALSE = "F"
  show (And left right) = "(" ++ show left ++ " & " ++ show right ++ ")"
  show (Or  left right) = "(" ++ show left ++ " v " ++ show right ++ ")"
  show (Imp left right) = "(" ++ show left ++ " --> " ++ show right ++ ")"

data Inference = Inference {
  delta :: [Expression],
  omega :: [Expression],
  pole :: POLE, 
  express :: Expression
} deriving
    (Eq)

eShow :: Expression -> String
eShow val = show val

pShow :: POLE -> String
pShow val = show val 

instance Show Inference where
  show Inference {delta = delt, omega = omeg, pole = lrc, express = expr} = 
    show (fmap eShow delt) 
    ++ "," 
    ++ show  (fmap eShow  omeg)
    ++ " |- " 
    ++ pShow  lrc
    ++ show (eShow expr)


