module Compile.FAST where 

import Compile.CAST

import Data.List

-- Field AST

data FieldOp a
  = FAdd (FieldExpr a) (FieldExpr a)
  | FSubtract (FieldExpr a) (FieldExpr a)
  | FMultiply (FieldExpr a) (FieldExpr a)
  | FDivide (FieldExpr a) (FieldExpr a)
  | FNthRootOfUnity Int deriving (Show,Eq)

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
    extractVarsOp (FNthRootOfUnity _) = []


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
    ro_fop_help :: FieldExpr a -> FieldExpr a -> ((Bool,Bool) -> FieldExpr a -> FieldExpr a -> FieldExpr a) -> FieldExpr a
    ro_fop_help e1 e2 f = let re1 = ro_expr e1 in
      let re2 = ro_expr e2 in
        f (is_one_expr re1,is_one_expr re2) re1 re2
    is_one_expr (Constant 1) = True
    is_one_expr _ = False


--
--addSubtraction :: FieldAST a -> FieldAST a

-- translation

-- builds from the bottom, assumes CExpr are already valid implementations of fieldExpr
class Field a where
  add :: a -> CExpr -> CExpr -> CExpr
  sub :: a -> CExpr -> CExpr -> CExpr
  mult :: a -> CExpr -> CExpr -> CExpr
  divide :: a -> CExpr -> CExpr -> CExpr
  get_type :: a -> CType
 

data FField = FField Int

instance Field FField where
  add (FField p) e1 e2 = CBinaryOp CModulo (CBinaryOp CAdd e1 e2) (Literal (IntLiteral p))
  sub (FField p) e1 e2 = CBinaryOp CModulo (CBinaryOp CSubtract e1 e2) (Literal (IntLiteral p))
  mult (FField p) e1 e2 = CBinaryOp CModulo (CBinaryOp CMultiply e1 e2) (Literal (IntLiteral p))
  divide (FField p) e1 e2 = CBinaryOp CModulo (CBinaryOp CDivide e1 e2) (Literal (IntLiteral p)) --
  get_type (FField p) = CInt
 
translateFieldToC :: Field a => a -> FieldAST a -> CProgram
translateFieldToC field (FieldAST stmts) =
  let assigns = (fmap (translateFieldStmt field) stmts) in
    CProgram assigns

translateFieldStmt :: Field a => a -> FieldStmt a -> CStatement
translateFieldStmt field (FieldAssignment ident fexpr) = CAssignment ident (translateFieldExpr field fexpr)

translateFieldExpr :: Field a => a -> FieldExpr a -> CExpr
translateFieldExpr field (Constant c) = Literal (IntLiteral c)
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

                                             
