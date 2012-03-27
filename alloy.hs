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
import LookupMonad

showAlloy :: [(String,[String])] -> AST -> IO ()

showAlloy env (Node (_,Spec) [(Node (_,Locals) locals), pre, program, post]) =
  do [fn] <- lookupM "alloy.analysisfile" env
     ls <- lookupM "alloy.analysislibraries" env
     [cp] <- lookupM "alloy.classpath" env
     putStrLn ("writing analysis file to " ++ fn)
     wpEnv <- calcWp program post
     writeFile fn ((showLibraries ls) ++ (showSpec locals pre wpEnv))
     report <- runAnalyzer cp fn
     putStrLn report
     checks <- parseOutput report
     putStrLn (showOutput wpEnv checks)

calcWp program post = 

  return $ zip ["test"++(show i) | i <- [1..] ] (wpx program [(post,[],"satisfy the postcondition")])

showLibraries :: [String] -> String
showLibraries ls = foldr (++) "" (map f ls) where f s = "open " ++ s ++ "\n"

showSpec locals pre wpEnv =
	   "open util/integer\n\n"
        ++ "pred true { no none }\n"
        ++ "pred false { some none }\n\n"
	++ "one sig State {\n " ++ (showDecls locals) ++ "\n}\n" 
	++ "one sig Const {\n " ++ (showConstDecls cs) ++ "\n}\n"
	++  (foldr (++) "" (map (showOblig env) wpEnv))
  where ts = declsToList locals
        cs = cvars ts pre
        env = ts ++ cs
        showOblig e (nm, (wp, path, goal)) = "\n/*\ngoal: " ++ goal ++ "\npath: " ++ (show path) ++ "\n*/\n" ++ "assert " ++ nm ++ " {\n" ++ (showA e (pre `implies` wp)) ++ "\n}\ncheck " ++ nm ++ "\n\n"

runAnalyzer classpath analysisFile = 
  do (exitcode, out, err) <- readProcessWithExitCode "java" ["-cp",classpath, "AlloyCmdLine", analysisFile] []
     case exitcode of 
      ExitSuccess -> return out
      (ExitFailure _) -> fail err 

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

showOutput :: [(String, Oblig)] -> [ (String, Maybe Instance) ] -> String
showOutput wpEnv checks = foldr (++) "" (map f checks)
  where f (name, Nothing) = ""
        f (name, Just (Instance eqns)) = "\nThere is a problem that requires your attention (" ++ name++").\n" ++ "When the program starts with the following initial state:\n\n" ++ (showInst eqns) ++ "\n" ++ (pathOf name wpEnv) 

pathOf :: String -> [(String, Oblig)] -> String

pathOf name env = case (lookup name env) of 
  Nothing -> "... Oops, there is no information about this check :("
  (Just (_,path,goal)) -> "it fails to " ++ goal ++ "\n"
                          ++ "while following the path that goes through\n" 
                          ++ (showPath path) ++ "\n"

showPath [] = ""
showPath [(AlexPn _ line col)] = "Line " ++ (show line) ++ " column " ++ (show col) 
showPath ((AlexPn _ line col):(r:rest)) = "Line " ++ (show line) ++ " column " ++ (show col) ++ " then\n" ++ (showPath (r:rest))

showInst [] = ""
showInst ((Set _ _):rest) = showInst rest
showInst ((Relation kind name tuples):rest) = 
  if kind == "State" 
  then name ++ "=" ++ (show tuples) ++"\n" ++ (showInst rest)
  else showInst rest


        
