module Alloy.Generator where
import List
import AST
import Data.Tree
import Data.Ord
import Alloy.WPC
import System.Process
import System.Exit
import IO
import Alloy.Output.Parser
import LookupMonad

showAlloy :: [(String,[String])] -> AST -> IO ()

showAlloy env (Node (_,Spec) [locals, pre, program, post]) =
  do [afn] <- lookupM "alloy.analysisfile" env
     [out] <- lookupM "alloy.analysisoutput" env
     ls <- lookupM "alloy.analysislibraries" env
     [cp] <- lookupM "alloy.classpath" env
     putStrLn ("Writing analysis file to " ++ afn)
     wpEnv <- calcWp program post
     writeFile afn ((showLibraries ls) ++ (showSpec locals pre wpEnv))
     report <- runAnalyzer cp afn out
     putStrLn report
     output <- readFile out
     checks <- parseOutput output
     putStrLn (showOutput wpEnv checks)

calcWp program post = 

  return $ zip ["test"++(show i) | i <- [1..] ] (wpx program [(post,[],"satisfy the postcondition")])

showLibraries :: [String] -> String
showLibraries ls = foldr (++) "" (map f ls) where f s = "open " ++ s ++ "\n"

showSpec :: AST -> AST -> [(String,Oblig)] -> String

showSpec locals pre wpEnv =
	   "open util/integer\n\n"
        ++ "pred true { no none }\n"
        ++ "pred false { some none }\n\n"
	++ "one sig State {\n " ++ (showA [] locals) ++ "\n}\n" 
	++ "one sig Const {\n " ++ (showConstDecls cs) ++ "\n}\n"
        ++ (constraints env) ++ "\n\n"
	++  (foldr (++) "" (map (showOblig env) wpEnv))
  where ts = declsToList (subForest locals)
        cs = cvars ts pre
        env = ts ++ cs
        showOblig e (nm, (wp, path, goal)) = "\n/*\ngoal: " ++ goal ++ "\npath: " ++ (show path) ++ "\n*/\n" ++ "assert " ++ nm ++ " {\n" ++ (showA e (pre `implies` wp)) ++ "\n}\ncheck " ++ nm ++ "\n\n"

runAnalyzer classpath analysisFile outputFile = 
  do (exitcode, out, err) <- readProcessWithExitCode "java" ["-cp",classpath, "AlloyCmdLine", analysisFile, outputFile] []
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
        f (_, NotEq) [x,y] = x ++ " != " ++ y
        f (_, Greater) [x,y] = x ++ " > " ++ y
        f (_, Less) [x,y] = x ++ " < " ++ y
        f (_, Geq) [x,y] = x ++ " >= " ++ y
        f (_, Leq) [x,y] = x ++ " <= " ++ y
        f (_, Conj) [x,y] = x ++ " and " ++ y
        f (_, Disj) [x,y] = "("++ x ++ " or " ++ y++")"
        f (_, Implies) [x,y] = "(" ++ x ++ " => " ++ y ++")"
        f (_, Range) [x,y] = "range[" ++ x ++ "," ++ y ++ "]"
        f (_, Join) xs = (showJoin xs) 
        f (_, Not) [x] = "!(" ++ x ++ ")"
        f (_, Neg) [x] = x ++ ".negate"
        f (_, Int x) [] = show x
        f (_, AST.True) [] = "true"
        f (_, AST.False) [] = "false"
        f (_, Const) [x] = x
        f (_, Type "int") [] = "Int"
        f (_, Type "nat") [] = "Int"
        f (_, Type n) [] = n
        f (_, ArrayType _ t) [] = if t == "int" then "seq Int" else ("seq " ++ t)
        f (_, Locals) decls = showDecls decls
	f (_, Declaration) [names, typ] = names ++ " : " ++ typ
        f (_, List) names = showNames names
        f (_, Sum) [decls, e] = "(sum " ++ decls ++ " | " ++ e ++ ")"
        f (_, All) [decls, e] = "(all " ++ decls ++ " | " ++ e ++ ")"
        f (_, No) [decls, e] = "(no " ++ decls ++ " | " ++ e ++ ")"
        f (_, String n) [] = 
	   case (lookup n env) of
		(Just (Node (_,Const) _)) -> "Const." ++ n
		(Just _) -> "State." ++ n
		Nothing -> n

showJoin = foldr f ""
  where f x [] = x 
        f x xs = "(" ++ x ++ ")." ++ "(" ++ xs ++ ")"

showDecls = foldr f ""  
  where 
    f d [] = d 
    f d ds = d ++ "," ++ ds

showNames = foldr f "" 
  where 
    f n [] = n 
    f n ns = n ++ "," ++ ns

showConstDecls :: [(String,AST)] -> String
showConstDecls [] = ""
showConstDecls [(s,n)] = s ++ " : " ++ (showA [] n)
showConstDecls ((s,n):ns) = s ++ " : " ++ (showA [] n) ++ ",\n" ++ (showConstDecls ns)

constraints :: Env -> String
constraints env = foldr (++) "" (map f env)
 where 
 f (n, Node (_, Type "nat") []) = "fact { all s : State | s."++n++".gte[0] }\n"
 f (n, Node (_, ArrayType k _) []) = "fact { all s : State | #s."++n++"=s."++k++"}\n"
 f _ = ""

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
        f (name, Just (Instance eqns)) = "\n("++name++") " ++ "When the program starts with:\n\n" ++ (showInst eqns) ++ "\n" ++ (pathOf name wpEnv) 

pathOf :: String -> [(String, Oblig)] -> String

pathOf name env = case (lookup name env) of 
  Nothing -> "... Oops, there is no information about this check :("
  (Just (_,[],goal)) -> "it fails to " ++ goal ++ "\n"
  (Just (_,path,goal)) -> "it fails to " ++ goal ++ "\n"
                          ++ "while following the path that goes through\n" 
                          ++ (showPath path) ++ "\n"

showPath [] = ""
showPath [(line,col)] = "Line " ++ (show line) ++ " column " ++ (show col) 
showPath ((line,col):(r:rest)) = "Line " ++ (show line) ++ " column " ++ (show col) ++ " then\n" ++ (showPath (r:rest))

showInst [] = ""
showInst ((Set _ _):rest) = showInst rest
showInst ((Relation kind name tuples):rest) = 
  if kind == "State" 
  then name ++ "=" ++ (showTuples (map tail tuples)) ++"\n" ++ (showInst rest)
  else showInst rest

{- a hack while we don't have a type environment: if the tuple is unary
treat it as a basic value, otherwise assume that it is an array.
-}

showTuples :: [Tuple] -> String
showTuples [] = "[]"
showTuples ts = if (length (head ts)) == 1 then (head (head ts)) else ("[" ++ (showArray ts) ++ "]")

{- 
a tuple that represents an array is a list of the form

[[i1,e1],[i2,e2],...,[in,en]]

where the 'i's and the 'e's are strings. to show it as an array we first
convert it into a list of pairs:

[ (i1,e1),...,(in,en) ]

where the 'i's are integers, then sort the pairs according to the index,
remove the index and finally show the (now) ordered list. 

-}

showArray :: [Tuple] -> String
showArray = printArray . removeIndices . sortPairs . convertIndexToInt 

convertIndexToInt :: [Tuple] -> [(Int, String)]
convertIndexToInt [] = []
convertIndexToInt ([key,e]:rest) = (read key,e) : (convertIndexToInt rest)

sortPairs :: [(Int, String)] -> [(Int, String)]
sortPairs = sortBy (comparing fst)

removeIndices = map snd 

printArray = foldr f "" 
  where f e [] = e
        f e xs = e ++ ", " ++ xs

