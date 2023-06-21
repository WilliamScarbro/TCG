module Compile.CAST where

data CProgram = CProgram [CStatement] deriving (Eq,Show)

-- C AST

data CStatement
  = CAssignment Identifier CExpr
  | CVarDeclare CType Identifier
  deriving (Eq,Show)
  -- | If CExpr [tatement] (Maybe [Statement])
  -- | While CExpr [Statement]
  -- | Return (Maybe CExpr)

data CExpr
  = CBinaryOp CBinaryOperator CExpr CExpr
  | CUnaryOp CUnaryOperator CExpr
  | Literal Literal
  | Identifier String
  deriving (Eq,Show)

data CBinaryOperator
  = CAdd
  | CSubtract
  | CMultiply
  | CDivide
  | CModulo
  deriving (Eq,Show)
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
  deriving (Eq,Show)

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
  deriving (Eq,Show)

get_stmts :: CProgram -> [CStatement]
get_stmts (CProgram stmts) = stmts
--

translateCToStr :: CProgram -> String
translateCToStr (CProgram stmts) = unlines (map translateStmt stmts)

translateStmt :: CStatement -> String
translateStmt (CAssignment ident expr) = ident ++ " = " ++ translateExpr expr ++ ";"
translateStmt (CVarDeclare ctype ident) = translateCType ctype ++ " " ++ ident ++ ";"
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

add_boiler_plate :: CProgram -> Int -> String
add_boiler_plate cp size = let prog = translateCToStr cp in
  "#include <stdio.h>\n\
  \#include <stdlib.h>\n\
  \#include \"../Util.h\"\n\
  \\n\
  \void gen(int* X, int* Y){\n"++prog++"\n}\n\
  \\n\
  \int main(int argc,char** argv){\n\
  \  int* X = malloc(sizeof(int)*"++show size++");\n\
  \  int* Y = malloc(sizeof(int)*"++show size++");\n\
  \\n\
  \  for(int i=0; i<"++show size++"; i++){\n\
  \    X[i]=i;\n\
  \  }\n\
  \  gen(X,Y);\n\
  \\n\
  \  print_array(\"result\",Y,"++show size++");\n\
  \\n\
  \  free(X);\n\
  \  free(Y);\n\
  \}\n"                               
