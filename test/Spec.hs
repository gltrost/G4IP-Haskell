module Main (main) where

import Test.Hspec
import Control.Exception (evaluate)
import Expression
import G4IP

main :: IO ()
main = hspec $ do
  describe "Provability of expressions:" $ do
    it (prettyExpr t1 ++ " is provable") $ do
      find t1 `shouldBe` True

    it (prettyExpr t2 ++ " is provable") $ do
      find t2 `shouldBe` True 

    it (prettyExpr t3 ++ " is provable") $ do
      find t3 `shouldBe` True 

    it (prettyExpr t4 ++ " is provable") $ do
      find t4 `shouldBe` True

    it (prettyExpr t5 ++ " is provable") $ do
      find t5 `shouldBe` True 

    it (prettyExpr t6 ++ " is unprovable") $ do
      find t6 `shouldBe` False 

    it (prettyExpr t7 ++ " is unprovable") $ do
      find t7 `shouldBe` False

    it (prettyExpr t8 ++ " is unprovable") $ do
      find t8 `shouldBe` False 

    it (prettyExpr t9 ++ " is unprovable") $ do
      find t9 `shouldBe` False 

    it (prettyExpr t10 ++ " is unprovable") $ do
      find t10 `shouldBe` False

    it (prettyExpr t11 ++ " is unprovable") $ do
      find t11 `shouldBe` False                 

-- Atoms 
p = Atom "p"
q = Atom "q" 
r = Atom "r"

-- Should come out to True
t1 = (p `Imp` p)
t2 = (p `Imp` (p `And` p))
t3 = TRUE
t4 = TRUE `And` (TRUE `Or` p)
t5 = ((p `Imp` q) `And` (q `Imp` r)) `Imp` (p `Imp` r)

-- Should come out to False
t6 = p `And` FALSE  
t7 = FALSE
t8 = (TRUE `Imp` FALSE)
t9 = (TRUE `And` FALSE)
t10 = p
t11 = FALSE `Or` FALSE

