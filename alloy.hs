module Alloy where
import List
import AST
import WPC

type Env = [(String,Node)]

showAlloy :: Node -> String

showAlloy = showA [] 

showA :: Env -> Node -> String

showA _ (Spec locals pre program post) = 
	   "open util/integer\n\n"
        ++ "pred true { no none }\n"
        ++ "pred false { some none }\n\n"
	++ "one sig State {\n " ++ (showA [] locals)  
	++ "\n}\n\none sig Const {\n " ++ (showConstDecls (cvar env pre))
	++ "\n}\n\npred obligation {\n" 
	++  "(" ++ (showA env pre) ++ ") => " 
	++  "(" ++ (showA env (wp program post)) ++ ")"
	++ "\n}\n\ncheck { obligation }\n"
  where env = types locals

showA _ (Locals ds) = showDecls ds 
showA env (Plus x y) = (showA env x) ++ ".add[" ++ (showA env y) ++ "]"
showA env (Minus x y) = (showA env x) ++ ".sub[" ++ (showA env y) ++ "]"
showA env (Times x y) = (showA env x) ++ ".mul[" ++ (showA env y) ++"]"
showA env (TypeVar vn) = if vn == "int" then "Int" else vn

showA env (Var vn) = 
	case (lookup vn env) of
		(Just _) -> "State." ++ vn
		Nothing -> "Const." ++ vn

showA _ (Nat x) = (show x)
showA env (Neg x) = "-" ++ (showA env x)
showA env (Quotient x y) = (showA env x) ++ " / " ++ (showA env y)
showA env (Div x y) = (showA env x) ++ ".div[" ++ (showA env y) ++"]"
showA env (Mod x y) = (showA env x) ++ ".rem[" ++ (showA env y) ++"]"
showA env (NodeEq x y) = (showA env x) ++ " = " ++ (showA env y)
showA env (NodeGeq x y) = (showA env x) ++ " >= " ++ (showA env y)
showA env (NodeLeq x y) = (showA env x) ++ " <= " ++ (showA env y)
showA env (Conj x y) = (showA env x) ++ " and " ++ (showA env y)
showA env (Disj x y) = "("++ (showA env x) ++ " or " ++ (showA env y)++")"
showA env (Implies x y) = "(" ++ (showA env x) ++ " => " ++ (showA env y) ++")"
showA _ (PredTrue) = "true"
showA _ (PredFalse) = "false"

showDecls [d] = showDecl d
showDecls (d:(dd:ds)) = (showDecl d) ++ ";" ++ (showDecls (dd:ds))

showDecl (Declaration ds tp) = (showNames ds) ++ " : " ++ (showA [] tp)
showNames [n] = n
showNames (n:(nn:ns)) = n ++ "," ++ (showNames (nn:ns))

showConstDecls :: [(String,Node)] -> String
showConstDecls [] = ""
showConstDecls [(s,n)] = s ++ " : " ++ (showA [] n)
showConstDecls ((s,n):ns) = s ++ " : " ++ (showA [] n) ++ ",\n" ++ (showConstDecls ns)

types :: Node -> Env

types (Locals ds) = declsToList ds

cvar :: [(String, Node)] -> Node -> [(String, Node)]

-- to find the type of a constant (a.k.a model) variable 
-- we look for equality expressions where the left hand side is
-- a state variable and the right hand side is the constant variable.

cvar types (NodeEq (Var v) (Var c)) = 
  case (lookup v types) of
    (Just t) -> case (lookup c types) of
      Nothing -> [(c, t)]
      (Just _) -> []
    Nothing -> []
	
cvar types (NodeEq x y) = (cvar types x) ++ (cvar types y)

cvar types (Var vn) = []
cvar types (Plus x y) = (cvar types x) ++ (cvar types y)
cvar types (Minus x y) = (cvar types x) ++ (cvar types y)
cvar types (Times x y) = (cvar types x) ++ (cvar types y)
cvar types (Nat x) = []
cvar types PredTrue = []
cvar types PredFalse = []
cvar types (Neg x) = cvar types x
cvar types (Quotient x y) = (cvar types x) ++ (cvar types y)
cvar types (Div x y) = (cvar types x) ++ (cvar types y)
cvar types (Mod x y) = (cvar types x) ++ (cvar types y)
cvar types (Conj x y) = (cvar types x) ++ (cvar types y)
cvar types (NodeGeq x y) = (cvar types x) ++ (cvar types y)
cvar types (NodeLeq x y) = (cvar types x) ++ (cvar types y)

