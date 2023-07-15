{-# LANGUAGE FlexibleInstances #-}

module Compile.CAST where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Util.Util
import Compile.Monty

data CProgram = CProgram [CStatement] deriving (Eq,Show)

-- C AST

instance Show (String -> [CStatement]) where
  show stmts_func = show (stmts_func "i")

instance Eq (String -> [CStatement]) where
  (==) f1 f2 = (==) (f1 "i") (f2 "i")

data CStatement
  = CAssignment Identifier CExpr
  | CVarDeclare CType Identifier
  | CLoop (Int,Int) (String -> [CStatement])
  deriving (Eq,Show)
  -- | If CExpr [tatement] (Maybe [Statement])
  -- | While CExpr [Statement]
  -- | Return (Maybe CExpr)

data CExpr
  = CBinaryOp CBinaryOperator CExpr CExpr
  | CUnaryOp CUnaryOperator CExpr
  | Literal Literal
  | Identifier String
  | CFunc Identifier [CExpr]
  deriving (Eq,Show)

data CBinaryOperator
  = CAdd
  | CSubtract
  | CMultiply
  | CDivide
  | CModulo
  deriving (Eq,Show,Ord)
--  | Equal
--  | NotEqual
--  | LessThan
--  | LessThanOrEqual
--  | GreaterThan
--  | GreaterThanOrEqual
--  | And
--  | Or

data CUnaryOperator
  = CNegate
  | CNot
  deriving (Eq,Show,Ord)

data Literal = IntLiteral Int
  deriving (Eq,Show)
--  | BoolLiteral Bool
--  | CharLiteral Char

data CType
  = CFloat
  | CDouble
  | CLong
  | CInt
  deriving (Eq,Show)
  
type Identifier = String

data COperator = CBinary CBinaryOperator | CUnary CUnaryOperator 
  deriving (Eq,Show,Ord)

get_stmts :: CProgram -> [CStatement]
get_stmts (CProgram stmts) = stmts
--

_multiply_string :: String -> Int -> String
_multiply_string str val = foldr (++) "" [str |i<-[0..val-1]]

--

translateCToStr :: CProgram -> [String]
translateCToStr (CProgram stmts) = map (translateStmt 0) stmts

translateStmt :: Int -> CStatement -> String
translateStmt depth (CAssignment ident expr) = (_multiply_string " " depth) ++ ident ++ " = " ++ translateExpr expr ++ ";"
translateStmt depth (CVarDeclare ctype ident) = (_multiply_string " " depth) ++ translateCType ctype ++ " " ++ ident ++ ";"
translateStmt depth (CLoop (start,end) stmts_func) =
  let
    index_var = "i"++show depth
    stmts = fmap (translateStmt (depth+1)) (stmts_func index_var)
  in
    (_multiply_string " " depth)++"for ("++index_var++"="++show start++"; "++index_var++"<"++show end++"; "++index_var++"++){"++
    (foldr (++) "" stmts)++
    "}"
    
-- translate If, While, Return statements


translateOp :: COperator -> String
translateOp (CBinary op) = translateBinaryOp op
translateOp (CUnary op) = translateUnaryOp op

translateBinaryOp :: CBinaryOperator -> String
translateBinaryOp CAdd = "+"
translateBinaryOp CSubtract = "-"
translateBinaryOp CMultiply = "*"
translateBinaryOp CDivide = "/"
translateBinaryOp CModulo = "%"

translateUnaryOp :: CUnaryOperator -> String
translateUnaryOp CNegate = "-"
translateUnaryOp CNot = "!"

translateExpr :: CExpr -> String
translateExpr (CBinaryOp op left right) = "(" ++ translateExpr left ++ " " ++ translateOp (CBinary op) ++ " " ++ translateExpr right ++ ")"
translateExpr (CUnaryOp op expr) = translateOp (CUnary op) ++ "(" ++ translateExpr expr ++ ")"
translateExpr (Literal l) = translateLiteral l
translateExpr (Identifier s) = s
translateExpr (CFunc name exprs) = name++"("++showStrTuple (fmap translateExpr exprs)++")"

translateLiteral :: Literal -> String
translateLiteral (IntLiteral x) = show x
--translateLiteral (IntLiteral n) = show n
--translateLiteral (FloatLiteral x) = show x
--translateLiteral (BoolLiteral b) = if b then "true" else "false"
--translateLiteral (CharLiteral c) = ['\'', c, '\'']

translateCType :: CType -> String
translateCType CDouble = "double"
translateCType CFloat = "float"
translateCType CLong = "long"
translateCType CInt = "int"

--
--

-- Function to count the number of operations in a C AST
countOperations :: CProgram -> Map COperator Int
countOperations (CProgram statements) = countStatements statements Map.empty
  where
    countStatements [] opCount = opCount
    countStatements (stmt:rest) opCount =
      let newOpCount = countStatement stmt opCount
      in countStatements rest newOpCount

    countStatement :: CStatement -> Map COperator Int -> Map COperator Int
    countStatement (CAssignment _ expr) opCount = countExpression expr opCount
    countStatement (CVarDeclare _ _) opCount = opCount -- Ignore variable declarations for counting operations

    countExpression :: CExpr -> Map COperator Int -> Map COperator Int
    countExpression (CBinaryOp op leftExpr rightExpr) opCount =
      let newOpCount = Map.insertWith (+) (CBinary op) 1 opCount
          updatedOpCount = countExpression leftExpr newOpCount
      in countExpression rightExpr updatedOpCount
    countExpression (CUnaryOp op expr) opCount =
      let newOpCount = Map.insertWith (+) (CUnary op) 1 opCount
      in countExpression expr newOpCount
    countExpression (Literal _) opCount = opCount
    countExpression (Identifier _) opCount = opCount
    countExpression (CFunc name exprs) opCount =
      foldr (\cur count -> countExpression cur count) opCount exprs
