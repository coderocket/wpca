module Alloy where
import List
import AST
import Data.Tree
import WPC
import Lexer
import System.Process
import System.Exit
import IO
import AnalysisOutputGrammar

runAnalyzer classpath analysisFile = 
  do (exitcode, out, err) <- readProcessWithExitCode "java" ["-cp",classpath, "AlloyCmdLine", analysisFile] []
     case exitcode of 
      ExitSuccess -> return out
      (ExitFailure _) -> return err 

showAlloy :: [(String,[String])] -> AST -> Maybe (IO ())

showAlloy env ast =
  do [fn] <- lookup "alloy.analysisfile" env
     ls <- lookup "alloy.analysislibraries" env
     [cp] <- lookup "alloy.classpath" env
     return $ do putStrLn ("writing analysis file to " ++ fn)
                 writeFile fn ((showLibraries ls) ++ (showSpec ast))
                 -- report <- runAnalyzer cp fn
		 -- putStrLn report
{-
		 case (parseOutput report) of 
                   (Left err) -> putStrLn ("failed to parse alloy's output: " ++ err)
                   (Right checks) -> putStrLn (showOutput checks)
-}

showLibraries :: [String] -> String
showLibraries ls = foldr (++) "" (map f ls) where f s = "open " ++ s ++ "\n"

showSpec (Node (_,Spec) [(Node (_,Locals) locals), pre, program, post]) = 
	   "open util/integer\n\n"
        ++ "pred true { no none }\n"
        ++ "pred false { some none }\n\n"
	++ "one sig State {\n " ++ (showDecls locals) ++ "\n}\n" 
	++ "one sig Const {\n " ++ (showConstDecls cs) ++ "\n}\n"
	++  (foldr (++) "" (map (showOblig env) (zip (wpx program [(post,[],"postcondition")]) [1..]))) 
  where ts = declsToList locals
        cs = cvars ts pre
        env = ts ++ cs
        showOblig e ((wp, path, goal), k) = "\n/*\ngoal: " ++ goal ++ "\npath: " ++ (show path) ++ "\n*/\n" ++ "assert " ++ nm ++ " {\n" ++ (showA e (pre `implies` wp)) ++ "\n}\ncheck " ++ nm ++ "\n\n"
          where nm = "test"++(show k)

showA :: Env -> AST -> String
showA env = foldRose f 
  where f (_, Plus) [x,y] = x ++ ".add[" ++ y ++ "]"
        f (_, Minus) [x,y] = x ++ ".sub[" ++ y ++ "]"
        f (_, Times) [x,y] = x ++ ".mul[" ++ y ++"]"
        f (_, Div) [x,y] = x ++ ".div[" ++ y ++"]"
        f (_, Mod) [x,y] = x ++ ".rem[" ++ y ++"]"
        f (_, Quotient) [x,y] = "(" ++ x ++ " / " ++ y ++ ")"
        f (_, Eq) [x,y] = x ++ " = " ++ y
        f (_, Greater) [x,y] = x ++ " > " ++ y
        f (_, Less) [x,y] = x ++ " < " ++ y
        f (_, Geq) [x,y] = x ++ " >= " ++ y
        f (_, Leq) [x,y] = x ++ " <= " ++ y
        f (_, Conj) [x,y] = x ++ " and " ++ y
        f (_, Disj) [x,y] = "("++ x ++ " or " ++ y++")"
        f (_, Implies) [x,y] = "(" ++ x ++ " => " ++ y ++")"
        f (_, Join) xs = (showJoin xs) 
        f (_, Not) [x] = "!(" ++ x ++ ")"
        f (_, Neg) [x] = x ++ ".negate"
        f (_, Int x) [] = show x
        f (_, AST.True) [] = "true"
        f (_, AST.False) [] = "false"
        f (_, Const) [x] = x
        f (_, Type n) [] = if n == "int" then "Int" else n
        f (_, String n) [] = 
	   case (lookup n env) of
		(Just (Node (_,Const) _)) -> "Const." ++ n
		(Just _) -> "State." ++ n
		Nothing -> n

showJoin = foldr f ""
  where f x [] = x 
        f x xs = "(" ++ x ++ ")." ++ "(" ++ xs ++ ")"

showDecls [d] = showDecl d
showDecls (d:ds) = (showDecl d) ++ ";" ++ (showDecls ds)

showDecl (Node (_,Declaration) [Node (_,List) ds, tp]) = (showNames ds) ++ " : " ++ (showA [] tp)

showNames [(Node (_,String n)[])] = n
showNames ((Node (_,String n)[]):ns) = n ++ "," ++ (showNames ns)

showConstDecls :: [(String,AST)] -> String
showConstDecls [] = ""
showConstDecls [(s,n)] = s ++ " : " ++ (showA [] n)
showConstDecls ((s,n):ns) = s ++ " : " ++ (showA [] n) ++ ",\n" ++ (showConstDecls ns)

cvars :: Env -> AST -> Env

-- To find the type of a constant (a.k.a model) variable 
-- we look for either a top level equality expression or 
-- an equality expression in top level conjunctions where 
-- the left hand side is a state variable and the right 
-- hand side is the constant variable.

cvars types (Node (_,Eq) [(Node (_,String v) []), (Node (p,String c) [])]) = 
  case (lookup v types) of
    (Just t) -> case (lookup c types) of
      Nothing -> [(c, Node (p, Const) [t])]
      (Just _) -> []
    Nothing -> []
	
cvars types (Node (_, Conj) [x, y]) = (cvars types x) ++ (cvars types y)
cvars types _ = [] 

showOutput :: [ ([Loc], Maybe Instance) ] -> String
showOutput checks = foldr (++) "" (map f checks)
  where f (path, Nothing) = ""
        f (path, Just (Instance eqns)) = showCounterExample path eqns

showCounterExample path eqns = "\nThe program fails to satisfy its postcondition given the following initial values:\n" ++ (showInst eqns) ++ "\nThe program will follow this path:\n" ++ (showPath path 1)

showPath [] n = ""
showPath (Loc kind line col:rest) n = (show n) ++ ". The " ++ kind ++ " at line " ++ (show line) ++ " column " ++ (show col) ++ "\n" ++ (showPath rest (n+1)) 

showInst [] = ""
showInst ((Set _ _):rest) = showInst rest
showInst ((Relation kind name tuples):rest) = 
  if kind == "State" 
  then name ++ "=" ++ (show tuples) ++"\n" ++ (showInst rest)
  else showInst rest


        
