module Main (main) where

import Test.Hspec
import Control.Exception (evaluate)
import Expression
import G4IP

main :: IO ()
main = hspec $ do
  describe "Provability of expressions:" $ do
-- Basic examples that should return True
    it (prettyExpr t1 ++ " is provable") $ do
      g4ip t1 `shouldBe` True

    it (prettyExpr t2 ++ " is provable") $ do
      g4ip t2 `shouldBe` True 

    it (prettyExpr t3 ++ " is provable") $ do
      g4ip t3 `shouldBe` True 

    it (prettyExpr t4 ++ " is provable") $ do
      g4ip t4 `shouldBe` True

    it (prettyExpr t5 ++ " is provable") $ do
      g4ip t5 `shouldBe` True 

-- Basic examples that should return False
    it (prettyExpr t6 ++ " is unprovable") $ do
      g4ip t6 `shouldBe` False 

    it (prettyExpr t7 ++ " is unprovable") $ do
      g4ip t7 `shouldBe` False

    it (prettyExpr t8 ++ " is unprovable") $ do
      g4ip t8 `shouldBe` False 

    it (prettyExpr t9 ++ " is unprovable") $ do
      g4ip t9 `shouldBe` False 

    it (prettyExpr t10 ++ " is unprovable") $ do
      g4ip t10 `shouldBe` False

    it (prettyExpr t11 ++ " is unprovable") $ do
      g4ip t11 `shouldBe` False    

-- More advanced examples
    it (prettyExpr t12 ++ " is provable") $ do
      g4ip t12 `shouldBe` True                        

-- Atoms 
p = Atom "p"
q = Atom "q" 
r = Atom "r"

-- Basic examples that should return True
t1 = (p `Imp` p)
t2 = (p `Imp` (p `And` p))
t3 = TRUE
t4 = TRUE `And` (TRUE `Or` p)
t5 = ((p `Imp` q) `And` (q `Imp` r)) `Imp` (p `Imp` r)

-- Basic examples that should return False
t6 = p `And` FALSE  
t7 = FALSE
t8 = (TRUE `Imp` FALSE)
t9 = (TRUE `And` FALSE)
t10 = p
t11 = FALSE `Or` FALSE

-- More advanced examples
t12 = ((p `And` q) `Imp` r) `Imp` p `Imp` q `Imp` r

