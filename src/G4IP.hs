module G4IP where

import Expression
import Debug.Trace


-- Decision procedure for the G4ip calculus

   -- Right Invertible Rules


-- andR :: Inference -> [Inference]
-- andR Inference {delta = delt, omega = omeg, pole = lrn, express = expr} = 
--   case (lrn, expr) of
--     (RIGHT, exprL `And` exprR) -> 
--       [Inference {delta=delt, omega = omeg, pole=lrn, express=exprL}, 
--        Inference {delta=delt, omega = omeg, pole=lrn, express=exprR}]
--     _ -> error "NotApplicable"


-- impR :: Inference -> [Inference] 
-- impR Inference {delta = delt, omega = omeg, pole = lrn, express = expr} = 
--   case (lrn, expr) of 
--     (RIGHT, exprL `Imp` exprR) -> 
--       [Inference{delta=delt , omega = exprR : omeg, pole=lrn, express = exprL}]
--     _ -> error "NotApplicable"

-- tR :: Inference -> Expression 
-- tR Inference {delta = _, omega = _, pole = lrn, express = expr} =
--   case (lrn, expr) of
--     (RIGHT, TRUE) -> TRUE
--     _ -> error "NotApplicable"   


-- --  Switching Rules

-- lRP :: Inference -> [Inference]
-- lRP Inference {delta = delt, omega = omeg, pole = lrn, express = expr}=
--   case (lrn) of
--     RIGHT -> [Inference {delta = delt, omega = omeg, 
--               pole = LEFT, express = expr}]
--     _ -> error "NotApplicable"


-- --    (*Left Invertible Rules*)
-- andL :: Inference -> [Inference]
-- andL Inference {delta = delt, omega = omeg, pole = lrn, express = expr}  =
--   case (lrn, omeg) of
--     (LEFT, (topL `And` topR) : omegs) ->  
--       [Inference {delta=delt, omega= topL : (topR : omegs),
--        pole=LEFT, express =expr}]
--     _ -> error "NotApplicable"

-- vL :: Inference -> [Inference]
-- vL  Inference {delta = delt, omega = omeg, pole = lrn, express = expr} =
--   case (lrn,omeg) of
--     (LEFT, (topL `Or` topR) : omegs) ->
--      [Inference {delta=delt, omega = topL : omegs,pole = LEFT,express = expr}, 
--       Inference {delta=delt, omega = topR : omegs,pole = LEFT,express = expr}]
--     _ -> error "NotApplicable"

-- fL :: Inference -> Expression
-- fL  Inference {delta = _, omega = omeg, pole = lrn, express = _} =
--   case (lrn,omeg) of
--     (LEFT, FALSE : _) -> TRUE
--     _ -> error "NotApplicable"

-- tL :: Inference -> [Inference]
-- tL  Inference {delta = delt, omega = omeg, pole = lrn, express = expr} =
--   case (lrn,omeg) of
--     (LEFT, TRUE : omegs) -> 
--       [Inference {delta = delt, omega = omegs, pole = LEFT, express = expr}]
--     _ -> error "NotApplicable"

-- -- Compound Left Invertible Rules

-- tImpL :: Inference -> [Inference]
-- tImpL Inference {delta = delt, omega = omeg, pole = lrn, express = expr} =
--   case (lrn,omeg) of
--     (LEFT, (TRUE `Imp` topR) : omegs) -> 
--       [Inference {delta = delt, omega = topR : omegs, 
--        pole = lrn, express = expr}]
--     _ -> error "NotApplicable"

-- andImpL Inference {delta = delt, omega = omeg, pole = lrn, express = expr} =
--   case (lrn,omeg) of
--     (LEFT, (left1 `And` left2) `Imp` right : omegs) -> 
--       [Inference {delta = delt, omega = left1 `Imp` (left2 `Imp` right) : omegs,
--        pole = LEFT, express = expr}]
--     _ -> error "NotApplicable"

-- vImpL Inference {delta = delt, omega = omeg, pole = lrn, express = expr} =
--   case (lrn,omeg) of
--     (LEFT, ((left1 `Or` left2) `Imp` right) : omegs) -> 
--        [Inference {delta = delt, 
--         omega = (left1 `Imp` right) : ((left2 `Imp` right) : omegs),
--         pole = lrn, express = expr}]
--     _ -> error "NotApplicable"

-- fImpL Inference {delta = delt, omega = omeg, pole = lrn, express = expr} =
--   case (lrn,omeg) of
--     (LEFT, (FALSE `Imp` topR) : omegs) -> 
--       [Inference {delta = delt, omega = omegs, pole = LEFT, express = expr}]
--     _ -> error "NotApplicable"


-- -- Shift n' Search Rules

-- shift Inference {delta = delt, omega = omeg, pole = lrn, express = expr} =
--   case (lrn,omeg) of
--     (LEFT, top : omegs) -> 
--       Inference {delta = top : delt, omega = omegs, pole = LEFT, express = expr}
--     _ -> error "NotApplicable"


-- search Inference {delta = delt, omega = omeg, pole = lrn, express = expr} =
--   case (lrn,omeg) of
--     (LEFT, []) -> 
--       Inference {delta = delt, omega = omeg, pole = NEITHER, express = expr}
--     _ -> error "NotApplicable"



-- Search Rules

-- isSameP :: Expression -> Expression -> Bool
-- isSameP expr1 expr2 = 
--   case (expr1, expr2) of 
--     (Atom a1 ,Atom a2) -> a1 == a2
--     (TRUE,TRUE) -> True
--     (FALSE,FALSE) -> True
--     (left1 `And` right1,left2 `And` right2) -> 
--       isSameP left1 left2 && isSameP right1 right2
--     (left1 `Or` right1,left2 `Or` right2) -> 
--       isSameP left1 left2 && isSameP right1 right2
--     (left1 `Imp` right1,left2 `Imp` right2) -> 
--       isSameP left1 left2 && isSameP right1 right2
--     _ -> False

helpInit :: [Expression] -> Expression -> Bool
helpInit exprList someExpr =
  case exprList of
    [] -> False
    (x : xs) -> x == someExpr  || helpInit xs someExpr 


-- init Inference {delta = delt, omega = omeg, pole = lrn, express = expr} =
--   case lrn of 
--     NEITHER -> if (helpInit delt expr) then True else error "NotApplicable"
--     _ -> error "NotApplicable"


-- vR1 Inference {delta = delt, omega = omeg, pole = lrn, express = expr} =
--   case (omeg,lrn,expr) of
--     ([], NEITHER, exprL `Or` _) -> 
--       [Inference {delta = delt, omega = [], pole = RIGHT, express = exprL}] 
--     (_,_,_) -> error "NotApplicable"


-- vR2  Inference {delta = delt, omega = omeg, pole = lrn, express = expr} =
--   case (omeg,lrn, expr) of
--     ([], NEITHER, _ `Or` exprR) ->
--       [Inference {delta = delt, omega = [], pole = RIGHT, express = exprR}] 
--     _ -> error "NotApplicable"

-- Compound Left Search Rules
pimplHelp :: [Expression] -> [Expression] -> (Bool,Expression,[Expression])
pimplHelp delt1 delt2 = 
  case delt1 of 
    [] -> (False, Atom "filler", [])
    (topL `Imp` topR : delts) -> 
      if helpInit delt2 topL then (True, topR, delts) 
      else let (x,y,z) = pimplHelp delts delt2 in (x,y, topL `Imp` topR : z)
    (j : delts) ->
      let (x,y,z) = pimplHelp delts delt2 in (x,y,j : z)


-- pImpL Inference {delta = delt, omega = omeg, pole = lrn, express = expr} =
--   case lrn of
--     NEITHER -> 
--       case pimplHelp delt delt  of 
--         (True,c,delts) -> [Inference {delta=delts,omega = [c],pole = LEFT,express = expr}]
--         (False,_,_) -> error "NotApplicable"
--     _ -> error "NotApplicable"


-- impImpL Inference {delta = delt, omega = omeg, pole = lrn, express = expr} =
--   case (lrn, delt) of
--     (NEITHER, (left1 `Imp` left2) `Imp` right : delts) -> 
--       [Inference {delta=delts,omega=[left2 `Imp` right,left1],pole=RIGHT,express=left2},
--        Inference {delta = delt, omega = [right], pole = LEFT, express = expr}]
--     _ -> error "NotApplicable"


-- Decides

minOf :: [Bool] -> Bool
minOf boolList =
  case boolList of
    [] -> False
    (False : _) -> False
    (True : []) -> True
    (True : xs) -> minOf xs


proofSearch :: Inference -> Bool
proofSearch Inference {delta = delt, omega = omeg, pole = lrn, express = expr} =
  case (delt, omeg, lrn, expr,helpInit delt expr) of 
    (_,_,RIGHT, exprL `And` exprR,_) -> --andR
      proofSearch Inference {delta=delt, omega = omeg, pole=lrn, express=exprL} &&
      proofSearch Inference {delta=delt, omega = omeg, pole=lrn, express=exprR}
    (_,_,RIGHT, exprL `Imp` exprR,_) -> --impR
      proofSearch Inference{delta=delt , omega = exprL : omeg, pole=lrn, express = exprR}
    (_,_,RIGHT, TRUE,_) -> --tR
      True
    (_,_,RIGHT,_,_) -> -- lRP 
      proofSearch Inference {delta = delt, omega = omeg, pole = LEFT, express = expr}
    (_,(topL `And` topR) : omegs,LEFT,_,_) -> -- andL   
      proofSearch Inference {delta=delt, omega= topL:(topR : omegs),pole=LEFT, express =expr}
    (_,(topL `Or` topR) : omegs, LEFT, _,_) -> --vL 
      proofSearch Inference {delta=delt, omega = topL : omegs,pole = LEFT,express = expr} &&
      proofSearch Inference {delta=delt, omega = topR : omegs,pole = LEFT,express = expr}
    (_,FALSE : _, LEFT, _,_) -> --fL 
      True
    (_, TRUE : omegs, LEFT, _,_) -> --tL
      proofSearch Inference {delta = delt, omega = omegs, pole = LEFT, express = expr}
    (_, (TRUE `Imp` topR) : omegs, LEFT, _,_) -> -- tImpL 
      proofSearch Inference {delta = delt, omega = topR : omegs, pole = lrn, express = expr}
    (_,(left1 `And` left2) `Imp` right : omegs,LEFT,_,_) -> -- andImpL 
      proofSearch Inference {delta = delt, omega = left1 `Imp` (left2 `Imp` right) : omegs,
       pole = LEFT, express = expr}
    (_,((left1 `Or` left2) `Imp` right) : omegs, LEFT, _,_) -> --vImpL 
      proofSearch Inference {delta = delt, 
      omega = (left1 `Imp` right) : ((left2 `Imp` right) : omegs),
      pole = lrn, express = expr}
    (_,(FALSE `Imp` _) : omegs, LEFT, _,_) -> --fImpL
      proofSearch Inference {delta = delt, omega = omegs, pole = LEFT, express = expr}
    (_,top : omegs, LEFT, _,_) -> -- shift
      proofSearch Inference {delta = top : delt, omega = omegs, pole = LEFT, express = expr}
    (_,[],LEFT,_,_) -> -- search 
      proofSearch Inference {delta = delt, omega = omeg, pole = NEITHER, express = expr}
    (_,_,NEITHER,_,True) -> --init
      True
    (_,[], NEITHER, exprL `Or` exprR,_) -> --vR2 and vrL combined
      proofSearch Inference {delta = delt, omega = [], pole = RIGHT, express = exprR} ||
      proofSearch Inference {delta = delt, omega = [], pole = RIGHT, express = exprL} 
    ((left1 `Imp` left2) `Imp` right : delts, _, NEITHER, _,_) -> --impImpL
        proofSearch Inference {delta=delts,omega=[left2 `Imp` right,left1],pole=RIGHT,express=left2} &&
        proofSearch Inference {delta = delt, omega = [right], pole = LEFT, express = expr}
    (_,_,NEITHER,_,_) -> --pImpL
      case pimplHelp delt delt of 
        (True,c,delts) -> proofSearch Inference {delta=delts,omega = [c],pole = LEFT,express = expr}
        (False,_,_) -> False
    _ -> False

find :: Expression -> Bool
find expr = 
  proofSearch Inference {delta = [], omega = [], pole = RIGHT, express = expr} 