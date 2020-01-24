module G4IP where

import Expression
import Debug.Trace


helpInit :: [Expression] -> Expression -> Bool
helpInit exprList someExpr =
  case exprList of
    [] -> False
    (x : xs) -> x == someExpr  || helpInit xs someExpr 

pimplHelp :: [Expression] -> [Expression] -> (Bool,Expression,[Expression])
pimplHelp delt1 delt2 = 
  case delt1 of 
    [] -> (False, Atom "filler", [])
    (topL `Imp` topR : delts) -> 
      if helpInit delt2 topL then (True, topR, delts) 
      else let (x,y,z) = pimplHelp delts delt2 in (x,y, topL `Imp` topR : z)
    (j : delts) ->
      let (x,y,z) = pimplHelp delts delt2 in (x,y,j : z)


minOf :: [Bool] -> Bool
minOf boolList =
  case boolList of
    [] -> False
    (False : _) -> False
    (True : []) -> True
    (True : xs) -> minOf xs

-- G4IP is structured as viewing sequent-expressions as a 4-tuple: 
  -- Delta,Omega -->p e   where 
  -- Delta and Omega are sets of expressions 
  -- p refers to the polarity of -->, either Left , Right or Neither 
  -- e is an expression
-- We begin with [],[] -->Right e, where e is the expression we hope to prove

find :: Inference -> Bool
find Inference {delta=delt , omega=omeg, pole=lrn, express=expr} =
  case (delt , omeg, lrn, expr,helpInit delt expr) of 
    (_,_,RIGHT , exprL `And` exprR,_) -> --andR
      find Inference {delta=delt , omega=omeg, pole=lrn, express=exprL} &&
      find Inference {delta=delt , omega=omeg, pole=lrn, express=exprR}
    (_,_,RIGHT , exprL `Imp` exprR,_) -> --impR
      find Inference{delta=delt , omega=exprL : omeg, pole=lrn, express=exprR}
    (_,_,RIGHT , TRUE,_) -> --tR
      True
    (_,_,RIGHT ,_,_) -> -- lRP 
      find Inference {delta=delt , omega=omeg, pole=LEFT , express=expr}
    (_,(topL `And` topR) : omegs,LEFT ,_,_) -> -- andL   
      find Inference {delta=delt , omega= topL:(topR : omegs) , 
        pole=LEFT , express=expr}
    (_,(topL `Or` topR) : omegs, LEFT , _,_) -> --vL 
      find Inference {delta=delt , omega=topL : omegs,pole=LEFT , 
        express=expr} &&
      find Inference {delta=delt , omega=topR : omegs,pole=LEFT , express=expr}
    (_,FALSE : _, LEFT , _,_) -> --fL 
      True
    (_, TRUE : omegs, LEFT , _,_) -> --tL
      find Inference {delta=delt , omega=omegs, pole=LEFT , express=expr}
    (_, (TRUE `Imp` topR) : omegs, LEFT , _,_) -> -- tImpL 
      find Inference {delta=delt , omega=topR : omegs, pole=lrn, express=expr}
    (_,(left1 `And` left2) `Imp` right : omegs,LEFT ,_,_) -> -- andImpL 
      find Inference {delta=delt , omega=left1 `Imp` (left2 `Imp` right) : omegs,
       pole=LEFT , express=expr}
    (_,((left1 `Or` left2) `Imp` right) : omegs, LEFT , _,_) -> --vImpL 
      find Inference {delta=delt , 
      omega=(left1 `Imp` right) : ((left2 `Imp` right) : omegs),
      pole=lrn , express=expr}
    (_,(FALSE `Imp` _) : omegs, LEFT , _,_) -> --fImpL
      find Inference {delta=delt , omega=omegs, pole=LEFT , express=expr}
    (_,top : omegs, LEFT , _,_) -> -- shift
      find Inference {delta= top : delt , omega=omegs, pole=LEFT , express=expr}
    (_,[],LEFT ,_,_) -> -- search 
      find Inference {delta=delt , omega=omeg, pole=NEITHER , express=expr}
    (_,_,NEITHER,_,True) -> --init
      True
    (_,[], NEITHER, exprL `Or` exprR,_) -> --vR2 and vrL combined
      find Inference {delta=delt , omega=[], pole=RIGHT , express=exprR} ||
      find Inference {delta=delt , omega=[], pole=RIGHT , express=exprL} 
    ((left1 `Imp` left2) `Imp` right : delts, _, NEITHER, _,_) -> --impImpL
        find Inference {delta=delts , omega=[left2 `Imp` right ,left1] , 
          pole=RIGHT , express=left2} &&
        find Inference {delta=delt , omega=[right], pole=LEFT , express=expr}
    (_,_,NEITHER,_,_) -> --pImpL
      case pimplHelp delt delt of 
        (True,c,delts) -> 
          find Inference {delta=delts,omega=[c],pole=LEFT ,express=expr}
        (False,_,_) -> False
    _ -> False

g4ip :: Expression -> Bool
g4ip expr = 
  find Inference {delta= [], omega=[], pole=RIGHT , express=expr} 