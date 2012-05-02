module Alloy.Analyzer where
import AST
import Data.Tree
import Config.Parser
import LookupMonad
import Alloy.WPC
import System.Process
import System.Exit
import Alloy.Output.Parser
import Data.Ord
import List

{- 

To analyse a wpca program we first generate an alloy model from the code,
then we run the alloy analyzer on the model and finally we generate a
report based on the results of the analysis.

-}

work :: Config -> AST -> IO ()

work cfg code = 
  do obligs <- generate cfg code
     analyse cfg 
     report cfg obligs

{- To generate the model we follow these steps:

1. augment every identifier with information about its nature 
    (state variable, const variable or quantifier variable) 
2. extract the state variables, the constants and the global constraints
3. calculate the proof obligations 
4. Retrieve the name of the analysis file from the configuration and
   write the model to the analysis file.

Note that we return the proof obligations because we need them to generate
the report.

-}

generate :: Config -> AST -> IO ([(String,Oblig)])

generate cfg code = 
  do putStrLn "Generating model..." 
     stateVars <- return $ getStateVars code
     constants <- return $ getConstants code 
     acode <- augment code
     obligs <- calcObligs acode
     [analysisFile] <- lookupM "alloy.analysisfile" cfg
     libraries <- lookupM "alloy.analysislibraries" cfg
     putStrLn (" ...Writing analysis file to " ++ analysisFile)
     writeFile analysisFile (showModel libraries stateVars constants obligs)
     return obligs

augment :: AST -> IO (AST)

augment code@(Node (pos,Spec) [locals, pre, program, post]) = return $ Node (pos,Spec) [locals, f initialEnv pre, f initialEnv program, f initialEnv post] 
  where f env (Node (pos, String n) []) = case (lookup n env) of
                                           (Just t) -> Node (pos, t) []
                                           Nothing -> Node (pos, String n) []
        f env (Node datum children) = Node datum (map (f env) children)
        initialEnv = [ (n, StateVar n) | (n,_) <- (getStateVars code) ] ++ [ (n, ConstVar n) | (n,_) <- getConstants code ]

getStateVars :: AST -> Env

getStateVars (Node (_,Spec) [locals, pre, program, post]) = 
  (declsToList (subForest locals)) 

getConstants (Node (_,Spec) [locals, pre, program, post]) = 
  cvars (declsToList (subForest locals)) pre

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

calcObligs (Node (_,Spec) [locals, pre, program, post]) = 
 return $ zip names [ (pre `implies` wp,path,goal) | (wp,path,goal) <- obligations]
 where names = ["test"++(show i) | i <- [1..] ] 
       obligations = wpx program [(post,[],"satisfy the postcondition")]

showModel :: [String] -> Env -> Env -> [(String,Oblig)] -> String

showModel libraries stateVars constants obligs =
 (foldr (++) "" [ "open " ++ s ++ "\n" | s <- libraries])
 ++ "one sig State {\n " ++ (showEnv stateVars) ++ "\n}\n" 
 ++ "one sig Const {\n " ++ (showEnv constants) ++ "\n}\n"
 ++ (showConstraints (stateVars ++ constants)) ++ "\n\n"
 ++ (foldr (++) "" (map showOblig obligs))

showConstraints :: Env -> String
showConstraints env = foldr (++) "" (map f env)
 where
 f (n, Node (_, Type "nat") []) = "fact { all s : State | s."++n++".gte[0] }\n"
 f (n, Node (_, ArrayType k _) []) = "fact { all s : State | #s."++n++"=s."++k++"}\n"
 f _ = ""
 
showOblig (nm, (wp, path, goal)) = "\n/*\ngoal: " ++ goal ++ "\npath: " ++ (show path) ++ "\n*/\n" ++ "assert " ++ nm ++ " {\n" ++ (showA wp) ++ "\n}\ncheck " ++ nm ++ "\n\n"

showEnv :: Env -> String
showEnv [] = ""
showEnv [(s,n)] = s ++ " : " ++ (showA n)
showEnv ((s,n):ns) = s ++ " : " ++ (showA n) ++ ",\n" ++ (showEnv ns)

showA :: AST -> String
showA = foldRose f
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
        f (_, Locals) decls = showNames decls
        f (_, Declaration) [names, typ] = names ++ " : " ++ typ
        f (_, List) names = showNames names
        f (_, Sum) [decls, e] = "(sum " ++ decls ++ " | " ++ e ++ ")"
        f (_, All) [decls, e] = "(all " ++ decls ++ " | " ++ e ++ ")"
        f (_, No) [decls, e] = "(no " ++ decls ++ " | " ++ e ++ ")"
        f (_, String n) [] = n
        f (_, StateVar n) [] = "State." ++ n
        f (_, ConstVar n) [] = "Const." ++ n

showJoin = foldr f ""
  where f x [] = x
        f x xs = "(" ++ x ++ ")." ++ "(" ++ xs ++ ")"

showNames = foldr f ""
  where
    f n [] = n
    f n ns = n ++ "," ++ ns

analyse :: Config -> IO ()

analyse cfg = 
  do [analysisFile] <- lookupM "alloy.analysisfile" cfg
     [outputFile] <- lookupM "alloy.analysisoutput" cfg
     [classpath] <- lookupM "alloy.classpath" cfg
     (exitcode, out, err) <- readProcessWithExitCode "java" ["-cp",classpath, "AlloyCmdLine", analysisFile, outputFile] []
     case exitcode of
      ExitSuccess -> putStrLn out
      (ExitFailure _) -> fail err


report :: Config -> [(String,Oblig)] -> IO ()

report cfg obligs = 
  do [outputFile] <- lookupM "alloy.analysisoutput" cfg
     output <- readFile outputFile
     checks <- parseOutput output
     putStrLn (showOutput obligs checks)

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

