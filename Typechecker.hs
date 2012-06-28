module Typechecker where
import AST
import Data.Tree
import Loc

intType = Node (startLoc, Type "int") []
boolType = Node (startLoc, Type "bool") []

typeof :: Env -> AST -> AST

typeof env (Node (_, Int _) []) = intType 
typeof env (Node (_, AST.True) []) = boolType 
typeof env (Node (_, AST.False) []) = boolType 
typeof env (Node (p, Plus) [x,y]) = binary "int" intType env p x y "add"
typeof env (Node (p, Minus) [x,y]) = binary "int" intType env p x y "subtract"
typeof env (Node (p, Times) [x,y]) = binary "int" intType env p x y "multiply"
typeof env (Node (p, Quotient) [x,y]) = binary "int" intType env p x y "divide"
typeof env (Node (p, Div) [x,y]) = binary "int" intType env p x y "divide"
typeof env (Node (p, Mod ) [x,y]) = binary "int" intType env p x y "modulo"
typeof env (Node (p, NotEq) [x,y]) = binary "int" boolType env p x y "compare (!=)"
typeof env (Node (p, Eq) [x,y]) = binary "int" boolType env p x y "compare (=)"
typeof env (Node (p, Geq) [x,y]) = binary "int" boolType env p x y "compare (>=)"
typeof env (Node (p, Leq) [x,y]) = binary "int" boolType env p x y "compare (<=)"
typeof env (Node (p, Greater) [x,y]) = binary "int" boolType env p x y "compare (>)"
typeof env (Node (p, Less) [x,y]) = binary "int" boolType env p x y "compare (<)"
typeof env (Node (p, Conj) [x,y]) = binary "bool" boolType env p x y "conjoin (and)" 
typeof env (Node (p, Disj) [x,y]) = binary "bool" boolType env p x y "disjoin (or)"
typeof env (Node (p, Implies) [x,y]) = binary "bool" boolType env p x y "(imply (=>)"
typeof env (Node (p, Not) [x]) = unary "bool" boolType env p x "negate (logical not)"
typeof env (Node (p, Neg) [x]) = unary "int" intType env p x "negate (-)"
typeof env (Node (p, String n) []) = 
  case (lookup n env) of
    (Just t) -> t
    Nothing -> error ("Type is missing: " ++ (show p) ++ ": for the variable " ++ n)

-- The following rule is specific for typechecking arrays. It must be modified to support
-- additional join expressions

typeof env (Node (p, ArrayJoin) [x,y]) = 
  case (typeof env x) of 
    (Node (_, Type "int") []) -> 
      case (typeof env y) of
        (Node (_, ArrayType _ t) []) -> Node (startLoc, Type t) []
        (Node (_, Const) [Node (_, ArrayType _ t) []]) -> Node (startLoc, Type t) []
        _ -> error ("Type mismatch: " ++ (show p) ++ ": attempt to de-reference a non-array")
    _ -> error ("Type mismatch: " ++ (show p) ++ ": array index must be an integer")

-- typeof (x.y) = T provided that
-- typeof x = X and typeof y = X <-> T

typeof env (Node (p, Join) [x,y]) = 
  case (typeof env y) of
    (Node (_, Product) [t1,t2]) -> if t1 == typeof env x 
                                   then t2
                                   else error ("Type mismatch at: " ++ (show p))
    _ -> error ("Type mismatch: " ++ (show p) ++ " the expression " ++ (show y) ++ " is not a relation.")

typeof _ uu = error ("unknown expression: " ++ (show uu))

unary :: String -> AST -> Env -> Loc -> AST -> String -> AST
unary argtype resulttype env p x opname = 
  case (typeof env x) of
    (Node (_,Type argtype) []) -> resulttype
    _ -> error ("Type mismatch: " ++ (show p) ++ ": attempt to " ++ opname ++ " a non " ++ argtype ++ " expression") 

binary :: String -> AST -> Env -> Loc -> AST -> AST -> String -> AST

binary argtype resulttype env p x y opname = 
  case (typeof env x) of 
   (Node (_, Type argtype) []) -> 
     case (typeof env y) of 
      (Node (_, Type argtype) []) -> resulttype
      _ -> error ("Type mismatch: " ++ (show p) ++ ": attempt to " ++ opname ++ " a non " ++ argtype ++ " expression")
   _ -> error ("Type mismatch: " ++ (show p) ++ ": attempt to " ++ opname ++ " a non " ++ argtype ++ " expression")

