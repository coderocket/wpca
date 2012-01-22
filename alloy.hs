module Alloy where
import List
import AST
import WPC
import System.Process
import System.Exit
import IO

runAnalyzer analysisFile = 
  do (exitcode, out, err) <- readProcessWithExitCode "java" ["-cp","C:\\Documents and Settings\\davidf\\My Documents\\src\\alloy4.1.10\\alloy4.jar;.", "AlloyCmdLine", analysisFile] []
     case exitcode of 
      ExitSuccess -> return out
      (ExitFailure _) -> return err 

showAlloy :: [(String,[String])] -> Node -> Maybe (IO ())

showAlloy env ast =
  do ns <- lookup "global.analysisfile" env
     ls <- lookup "global.analysislibraries" env
     return $ do putStrLn ("writing analysis file to " ++ (head ns))
                 writeFile (head ns) ((showLibraries ls) ++ (showA [] ast))
                 report <- runAnalyzer (head ns)
		 putStrLn report

showLibraries :: [String] -> String
showLibraries ls = foldr (++) "" (map f ls) where f s = "open " ++ s ++ "\n"

type Env = [(String,Node)]

showA :: Env -> Node -> String

showA _ (Spec locals pre program post) = 
	   "open util/integer\n\n"
        ++ "pred true { no none }\n"
        ++ "pred false { some none }\n\n"
	++ "one sig State {\n " ++ (showA [] locals) ++ "\n}\n" 
	++ "one sig Const {\n " ++ (showConstDecls cs) ++ "\n}\n"
	++  (foldr (++) "" (map (showOblig env) (wp program [post]))) 
  where ts = types locals
        cs = cvars ts pre
        env = ts ++ cs
        showOblig e p = "check {\n" ++ (showA e (pre `implies` p)) ++ "\n}\n"

showA _ (Locals ds) = showDecls ds 
showA env (BinOp Plus x y) = (showA env x) ++ ".add[" ++ (showA env y) ++ "]"
showA env (BinOp Minus x y) = (showA env x) ++ ".sub[" ++ (showA env y) ++ "]"
showA env (BinOp Times x y) = (showA env x) ++ ".mul[" ++ (showA env y) ++"]"
showA env (TypeVar vn) = if vn == "int" then "Int" else vn

showA env (Var vn) = 
	case (lookup vn env) of
		(Just (Const _)) -> "Const." ++ vn
		(Just _) -> "State." ++ vn
		Nothing -> vn

showA env (Const n) = showA env n
showA _ (Nat x) = (show x)
showA env (Neg x) = (showA env x) ++ ".negate"
showA env (BinOp Quotient x y) = (showA env x) ++ " / " ++ (showA env y)
showA env (BinOp Div x y) = (showA env x) ++ ".div[" ++ (showA env y) ++"]"
showA env (BinOp Mod x y) = (showA env x) ++ ".rem[" ++ (showA env y) ++"]"
showA env (BinOp NodeEq x y) = (showA env x) ++ " = " ++ (showA env y)
showA env (BinOp NodeGreater x y) = (showA env x) ++ " > " ++ (showA env y)
showA env (BinOp NodeLess x y) = (showA env x) ++ " < " ++ (showA env y)
showA env (BinOp NodeGeq x y) = (showA env x) ++ " >= " ++ (showA env y)
showA env (BinOp NodeLeq x y) = (showA env x) ++ " <= " ++ (showA env y)
showA env (BinOp Conj x y) = (showA env x) ++ " and " ++ (showA env y)
showA env (BinOp Disj x y) = "("++ (showA env x) ++ " or " ++ (showA env y)++")"
showA env (BinOp Implies x y) = "(" ++ (showA env x) ++ " => " ++ (showA env y) ++")"
showA env (BinOp Join x y) = "("++(showA env x) ++ ").(" ++ (showA env y) ++ ")"
showA _ (PredTrue) = "true"
showA _ (PredFalse) = "false"
showA env (Not x) = "!(" ++ (showA env x) ++ ")"

showDecls [d] = showDecl d
showDecls (d:ds) = (showDecl d) ++ ";" ++ (showDecls ds)

showDecl (Declaration ds tp) = (showNames ds) ++ " : " ++ (showA [] tp)
showNames [n] = n
showNames (n:ns) = n ++ "," ++ (showNames ns)

showConstDecls :: [(String,Node)] -> String
showConstDecls [] = ""
showConstDecls [(s,n)] = s ++ " : " ++ (showA [] n)
showConstDecls ((s,n):ns) = s ++ " : " ++ (showA [] n) ++ ",\n" ++ (showConstDecls ns)

types :: Node -> Env

types (Locals ds) = declsToList ds

cvars :: [(String, Node)] -> Node -> [(String, Node)]

-- To find the type of a constant (a.k.a model) variable 
-- we look for either a top level equality expression or 
-- an equality expression in top level conjunctions where 
-- the left hand side is a state variable and the right 
-- hand side is the constant variable.

cvars types (BinOp NodeEq (Var v) (Var c)) = 
  case (lookup v types) of
    (Just t) -> case (lookup c types) of
      Nothing -> [(c, Const t)]
      (Just _) -> []
    Nothing -> []
	
cvars types (BinOp Conj x y) = (cvars types x) ++ (cvars types y)
cvars types _ = [] 

