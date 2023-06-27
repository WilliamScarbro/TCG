module Compile.FAST where 

import Compile.CAST
import Compile.Monty

import Data.List

-- Field AST

data FieldOp a
  = FAdd (FieldExpr a) (FieldExpr a)
  | FSubtract (FieldExpr a) (FieldExpr a)
  | FMultiply (FieldExpr a) (FieldExpr a)
  | FDivide (FieldExpr a) (FieldExpr a)
  | FNeg (FieldExpr a) deriving (Show,Eq)
  -- | FNthRootOfUnity Int deriving (Show,Eq)

data FieldExpr a
  = Constant Int -- hack, might be a problem later (might need double constants for \mathbb{C})
  | Variable String
  | FieldOpExpr (FieldOp a) deriving (Show,Eq)

data FieldStmt a = FieldAssignment Identifier (FieldExpr a) deriving (Show,Eq)

data FieldAST a = FieldAST [FieldStmt a] deriving (Show,Eq)

getVars :: FieldAST a -> [Identifier]
getVars (FieldAST stmts) = nub $ concatMap extractVars stmts
  where
    extractVars (FieldAssignment ident fexpr) = extractVarsExpr fexpr ++ [ident]
    extractVarsExpr (Constant _) = []
    extractVarsExpr (Variable v) = [v]
    extractVarsExpr (FieldOpExpr op) = extractVarsOp op
    extractVarsOp (FAdd fe1 fe2) = extractVarsExpr fe1 ++ extractVarsExpr fe2
    extractVarsOp (FSubtract fe1 fe2) = extractVarsExpr fe1 ++ extractVarsExpr fe2
    extractVarsOp (FMultiply fe1 fe2) = extractVarsExpr fe1 ++ extractVarsExpr fe2
    extractVarsOp (FDivide fe1 fe2) = extractVarsExpr fe1 ++ extractVarsExpr fe2
    --extractVarsOp (FNthRootOfUnity _) = []


-- optimization
removeZeros :: FieldAST a -> FieldAST a
removeZeros (FieldAST stmts) = FieldAST (fmap rz_stmt stmts)
  where
    rz_stmt (FieldAssignment id expr) = (FieldAssignment id (rz_expr expr))
    rz_expr (FieldOpExpr fo) = rz_fop fo
    rz_expr x = x
    rz_fop (FAdd e1 e2) = rz_fop_help e1 e2 addf
      where addf (iz1,iz2) re1 re2 = head $
              [ Constant 0 | (iz1,iz2)==(True,True) ] ++
              [ re2 | (iz1,iz2)==(True,False) ] ++
              [ re1 | (iz1,iz2)==(False,True) ] ++
              [ FieldOpExpr (FAdd re1 re2) ]
    rz_fop (FSubtract e1 e2) = rz_fop_help e1 e2 subf
      where subf (iz1,iz2) re1 re2 = head $
              [ Constant 0 | (iz1,iz2)==(True,True) ] ++
              --[  | (iz1,iz2)==(True,False) ] ++
              [ re1 | (iz1,iz2)==(False,True) ] ++
              [ FieldOpExpr (FSubtract re1 re2) ]
    rz_fop (FMultiply e1 e2) = rz_fop_help e1 e2 multf
      where multf (iz1,iz2) re1 re2 = head $
              [ FieldOpExpr (FMultiply re1 re2) | (iz1,iz2)==(False,False) ] ++
              [ Constant 0 ]
    rz_fop (FDivide e1 e2) = rz_fop_help e1 e2 divf
      where divf (iz1,iz2) re1 re2 = head $
              [ Constant 0 | (iz1,iz2)==(True,False) ] ++
              [ FieldOpExpr (FDivide re1 re2) ] -- don't handle div by zero errors here
    rz_fop_help :: FieldExpr a -> FieldExpr a -> ((Bool,Bool) -> FieldExpr a -> FieldExpr a -> FieldExpr a) -> FieldExpr a
    rz_fop_help e1 e2 f = let re1 = rz_expr e1 in
      let re2 = rz_expr e2 in
        f (is_zero_expr re1,is_zero_expr re2) re1 re2
    is_zero_expr (Constant 0) = True
    is_zero_expr _ = False


--
removeOnes :: FieldAST a -> FieldAST a
removeOnes (FieldAST stmts) = FieldAST (fmap ro_stmt stmts)
  where
    ro_stmt (FieldAssignment id expr) = (FieldAssignment id (ro_expr expr))
    ro_expr (FieldOpExpr fo) = ro_fop fo
    ro_expr x = x
    ro_fop (FAdd e1 e2) = ro_fop_help e1 e2 addf
      where addf _ re1 re2 = FieldOpExpr (FAdd re1 re2)
    ro_fop (FSubtract e1 e2) = ro_fop_help e1 e2 subf
      where subf _ re1 re2 = FieldOpExpr (FSubtract re1 re2)
    ro_fop (FMultiply e1 e2) = ro_fop_help e1 e2 multf
      where multf (io1,io2) re1 re2 = head $
              [ Constant 1 | (io1,io2)==(True,True) ] ++
              [ re2 | (io1,io2)==(True,False) ] ++
              [ re1 | (io1,io2)==(False,True) ] ++
              [ FieldOpExpr (FMultiply re1 re2) ]
    ro_fop (FDivide e1 e2) = ro_fop_help e1 e2 divf
      where divf (io1,io2) re1 re2 = head $
              [ Constant 1 | (io1,io2)==(True,True) ] ++
              --[  | (io1,io2)==(True,False) ] ++
              [ re1 | (io1,io2)==(False,True) ] ++
              [ FieldOpExpr (FDivide re1 re2) ]
    -- should be no negations at this point
    ro_fop_help :: FieldExpr a -> FieldExpr a -> ((Bool,Bool) -> FieldExpr a -> FieldExpr a -> FieldExpr a) -> FieldExpr a
    ro_fop_help e1 e2 f = let re1 = ro_expr e1 in
      let re2 = ro_expr e2 in
        f (is_one_expr re1,is_one_expr re2) re1 re2
    is_one_expr (Constant 1) = True
    is_one_expr _ = False

addNegation :: Field a => a -> FieldAST a -> FieldAST a
addNegation field (FieldAST stmts) = FieldAST (fmap (an_stmt field) stmts)
  where
    an_stmt field (FieldAssignment id expr) = (FieldAssignment id (an_expr field expr))
    an_expr field (FieldOpExpr fo) = add_negation field fo
    an_expr field x = x

    add_negation :: Field a => a -> FieldOp a -> FieldExpr a
    add_negation field (FMultiply e1 e2) = add_neg_help field e1 e2 add_neg_mult
      where
        add_neg_mult (ino1,ino2) e1 e2 = head $
          [ Constant 1 | (ino1,ino2)==(True,True)] ++
          [ FieldOpExpr (FNeg e2) | (ino1,ino2)==(True,False)] ++
          [ FieldOpExpr (FNeg e1) | (ino1,ino2)==(False,True)] ++
          [ FieldOpExpr (FMultiply e1 e2) ]
    add_negation field (FDivide e1 e2) = add_neg_help field e1 e2 add_neg_div
      where
        add_neg_div (ino1,ino2) e1 e2 = head $
          [ Constant 1 | (ino1,ino2)==(True,True)] ++
          [ FieldOpExpr (FNeg e1) | (ino1,ino2)==(False,True)] ++
          [ FieldOpExpr (FDivide e1 e2) ]
    add_negation field (FAdd e1 e2) = add_neg_help field e1 e2 add_neg_add
      where add_neg_add _ re1 re2 = FieldOpExpr (FAdd re1 re2)
    add_negation field (FSubtract e1 e2) = add_neg_help field e1 e2 add_neg_sub
      where add_neg_sub _ re1 re2 = FieldOpExpr (FSubtract re1 re2)
    add_negation field (FNeg e) = (FieldOpExpr (FNeg (an_expr field e)))

    add_neg_help :: Field a => a -> FieldExpr a -> FieldExpr a -> ((Bool,Bool) -> FieldExpr a -> FieldExpr a -> FieldExpr a) -> FieldExpr a
    add_neg_help field e1 e2 f = f (is_neg_one field e1,is_neg_one field e2) (an_expr field e1) (an_expr field e2)
    
removeNegation :: FieldAST a -> FieldAST a
removeNegation (FieldAST stmts) = FieldAST (fmap rn_stmt stmts)
  where
    rn_stmt (FieldAssignment id expr) = (FieldAssignment id (rn_expr expr))
    rn_expr (FieldOpExpr fo) = remove_negation fo
    rn_expr x = x

    remove_negation :: FieldOp a -> FieldExpr a
    remove_negation (FAdd e1 e2) = rem_neg_help e1 e2 rem_neg_add
      where
        rem_neg_add (ineg1,ineg2) e1 e2 = head $
          [ FieldOpExpr (FNeg (FieldOpExpr (FAdd e1 e2))) | (ineg1,ineg2)==(True,True) ] ++
          [ FieldOpExpr (FSubtract e1 e2) | (ineg1,ineg2)==(False,True) ] ++
          [ FieldOpExpr (FSubtract e2 e1) | (ineg1,ineg2)==(True,False) ] ++
          [ FieldOpExpr (FAdd e1 e2) ]
    remove_negation (FSubtract e1 e2) = rem_neg_help e1 e2 rem_neg_sub
      where
        rem_neg_sub (ineg1,ineg2) e1 e2 = head $
          [ FieldOpExpr (FSubtract e2 e1) | (ineg1,ineg2)==(True,True) ] ++
          [ FieldOpExpr (FAdd e1 e2) | (ineg1,ineg2)==(False,True) ] ++
          [ FieldOpExpr (FNeg (FieldOpExpr (FAdd e1 e2))) | (ineg1,ineg2)==(True,False) ] ++
          [ FieldOpExpr (FSubtract e1 e2) ]
    remove_negation (FMultiply e1 e2) = rem_neg_help e1 e2 rem_neg_mult
      where rem_neg_mult _ re1 re2 = FieldOpExpr (FMultiply re1 re2)
    remove_negation (FDivide e1 e2) = rem_neg_help e1 e2 rem_neg_div
      where rem_neg_div _ re1 re2 = FieldOpExpr (FDivide re1 re2)
    remove_negation (FNeg e) = (FieldOpExpr (FNeg (rn_expr e)))

    rem_neg_help :: FieldExpr a -> FieldExpr a -> ((Bool, Bool) -> FieldExpr a -> FieldExpr a -> FieldExpr a) -> FieldExpr a
    rem_neg_help expr1 expr2 f =
      let rn_expr1 = rn_expr expr1
          rn_expr2 = rn_expr expr2 in
        f (isNeg rn_expr1, isNeg rn_expr2) (unwrapNeg rn_expr1) (unwrapNeg rn_expr2)
      where
        isNeg (FieldOpExpr (FNeg _)) = True
        isNeg _ = False
        unwrapNeg (FieldOpExpr (FNeg expr)) = expr
        unwrapNeg expr = expr
    
-- translation

-- builds from the bottom, assumes CExpr are already valid implementations of fieldExpr
class Field a where
  add :: a -> CExpr -> CExpr -> CExpr
  sub :: a -> CExpr -> CExpr -> CExpr
  mult :: a -> CExpr -> CExpr -> CExpr
  divide :: a -> CExpr -> CExpr -> CExpr
  neg :: a -> CExpr -> CExpr
  get_type :: a -> CType
  is_neg_one :: a -> FieldExpr a -> Bool
  fieldElFromInt :: a -> Int -> CExpr
 

data FField = FField Int

instance Field FField where
  add (FField p) e1 e2 = CBinaryOp CModulo (CBinaryOp CAdd e1 e2) (Literal (IntLiteral p))
  sub (FField p) e1 e2 = CBinaryOp CModulo (CBinaryOp CSubtract e1 e2) (Literal (IntLiteral p))
  mult (FField p) e1 e2 = CBinaryOp CModulo (CBinaryOp CMultiply e1 e2) (Literal (IntLiteral p))
  divide (FField p) e1 e2 = CBinaryOp CModulo (CBinaryOp CDivide e1 e2) (Literal (IntLiteral p)) --
  neg (FField p) e = CUnaryOp CNegate e
  get_type (FField p) = CInt
  is_neg_one (FField p) (Constant c) = mod c p == p-1
  is_neg_one ff _ = False
  fieldElFromInt ff c = Literal (IntLiteral c)
  
instance Field Monty where
  add monty e1 e2 = CBinaryOp CAdd e1 e2
  sub monty e1 e2 = CBinaryOp CSubtract e1 e2
  mult monty e1 e2 = CFunc "REDC" [Identifier "monty",CBinaryOp CMultiply e1 e2] -- we assume there is a struct "monty" which has the necessary values
  divide monty e1 e2 = CFunc "NotImplemented" []
  neg monty e = CUnaryOp CNegate e
  get_type monty = CInt
  is_neg_one monty (Constant c) = mod c (p monty) == (p monty)-1
  is_neg_one monty _ = False
  fieldElFromInt monty c = Literal (IntLiteral (int2Residue monty c))
  
translateFieldToC :: Field a => a -> FieldAST a -> CProgram
translateFieldToC field (FieldAST stmts) =
  let assigns = (fmap (translateFieldStmt field) stmts) in
    CProgram assigns

translateFieldStmt :: Field a => a -> FieldStmt a -> CStatement
translateFieldStmt field (FieldAssignment ident fexpr) = CAssignment ident (translateFieldExpr field fexpr)

translateFieldExpr :: Field a => a -> FieldExpr a -> CExpr
translateFieldExpr field (Constant c) = fieldElFromInt field c
translateFieldExpr field (Variable v) = Identifier v
translateFieldExpr field (FieldOpExpr fop) = translateFieldOp field fop

translateFieldOp_help :: Field a => a -> FieldExpr a -> FieldExpr a -> (a -> CExpr -> CExpr -> CExpr) -> CExpr
translateFieldOp_help field fe1 fe2 func = let ce1 = translateFieldExpr field fe1 in
  let ce2 = translateFieldExpr field fe2 in
    func field ce1 ce2
    
translateFieldOp :: Field a => a -> FieldOp a -> CExpr
translateFieldOp field (FAdd fe1 fe2) = translateFieldOp_help field fe1 fe2 add
translateFieldOp field (FSubtract fe1 fe2) = translateFieldOp_help field fe1 fe2 sub
translateFieldOp field (FMultiply fe1 fe2) = translateFieldOp_help field fe1 fe2 mult
translateFieldOp field (FDivide fe1 fe2) = translateFieldOp_help field fe1 fe2 divide
translateFieldOp field (FNeg fe) = neg field (translateFieldExpr field fe)
                                             
-- Montgomery's Trick

--translateFieldToC_monty :: Field a => a -> Monty -> FieldAST a -> CProgram
--translateFieldToC_monty field monty (FieldAST stmts) =
--  let c_stmts = fmap (translateFieldStmt_monty field monty) stmts in
--    CProgram c_stmts
--
--translateFieldStmt_monty :: Field a => a -> Monty -> FieldStmt a -> CStatement
--translateFieldStmt_monty field monty (FieldAssignment ident fexpr) = CAssignment ident (traslateFieldExpr_monty field monty fexpr)
--
--translateFieldExpr_monty :: Field a => a -> Monty -> FieldExpr a -> CExpr
--translateFieldExpr_monty field monty (Constant c) = Literal (IntLiteral c)
--translateFieldExpr_monty field monty (Variable V) = Identifier v
--translateFieldExpr_monty field monty (FieldOpExpr fop) = translateFieldOp_monty field monty fop
--
--translateFieldOp_maybe :: Field a => a -> FieldOp a -> CExpr
--translateFieldOp field (FAdd fe1 fe2) = translateFieldOp_help field fe1 fe2 add
--translateFieldOp field (FSubtract fe1 fe2) = translateFieldOp_help field fe1 fe2 sub
--translateFieldOp field (FMultiply fe1 fe2) = translateFieldOp_help field fe1 fe2 mult
--translateFieldOp field (FDivide fe1 fe2) = translateFieldOp_help field fe1 fe2 divide
--translateFieldOp field (FNeg fe) = neg field (translateFieldExpr field fe)
--
--
