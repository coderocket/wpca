module Alloy.Analyzer where
import AST
import Data.Tree
import Config.Parser
import LookupMonad
import Alloy.WPC
import Typechecker
import System.Process
import System.Exit
import System.IO
import System.IO.Error
import Alloy.Output.Parser
import Data.Ord
import List

{- 

A WPCA program consists of a set of record definitions and a set of
procedure definitions. The record definitions are global to all the
procedures but the analysis of each procedure is independent of the
other procedures. Therefore, to analyze a WPCA program we analyze 
each procedure independently, passing it the global set of record
definitions.

-}

work :: Config -> AST -> IO ()
work cfg (Node (_,Program) [records, Node (_,List) procs, theory]) = loop procs
  where loop [] = return ()
        loop (p:ps) = do analyzeProc cfg records theory p 
                         loop ps

analyzeProc :: Config -> AST-> AST -> AST -> IO ()
analyzeProc cfg records theory (Node (p,Proc name) [params,pre,body,post]) = 
  do [analysisFile] <- lookupM "alloy.analysisfile" cfg 
     analyzeSpec (("alloy.analysisfile", [name ++ "." ++ analysisFile]):cfg) (Node (p, Spec) [params,pre,body,post])

{-

To analyse a wpca procedure we first generate an alloy model from
the code, then we run the alloy analyzer on the model and finally
we generate a report based on the results of the analysis.

-}

analyzeSpec :: Config -> AST -> IO ()

analyzeSpec cfg code = 
  do (obligs,types) <- generate cfg code
     analyse cfg 
     report types cfg obligs

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

generate :: Config -> AST -> IO ([(String,Oblig)], Env)

generate cfg code = 
  do putStrLn "Generating model..." 
     stateVars <- return $ getStateVars code
     constants <- return $ getConstants code 
     acode <- augment (tidyJoins code)
     obligs <- calcObligs acode
     [analysisFile] <- lookupM "alloy.analysisfile" cfg
     libraries <- lookupM "alloy.analysislibraries" cfg
     putStrLn ("... There are " ++ (show (length obligs)) ++ " proof obligations. Writing analysis file to " ++ analysisFile)
     writeFile analysisFile (showModel libraries stateVars constants obligs)
     return (obligs, stateVars ++ constants)

-- Alloy does not care if the join is due to an array or due to a relation

tidyJoins :: AST -> AST
tidyJoins = foldRose f 
  where f (pos, ArrayJoin) es = Node (pos, Join) es
        f k es = Node k es

augment :: AST -> IO (AST)

augment code@(Node (pos,Spec) [locals, pre, program, post]) = 
  return $ Node (pos,Spec) [locals, f [] initEnv pre, f [] initEnv program, f [] initEnv post] 
  where f bound env (Node (pos, String n) []) = 
          case (elemIndex n bound) of 
           Nothing -> case (lookup n env) of
                       (Just t) -> Node (pos, t) []
                       Nothing -> Node (pos, String n) []
           (Just _) -> Node (pos, String n) []
        f bound env (Node (pos, Quantifier q) [decls, body]) = 
          augmentQuantifier bound env pos q decls body
        f bound env (Node datum children) = 
           Node datum (map (f bound env) children)
        initEnv = [ (n, StateVar n) | (n,_) <- (getStateVars code) ] ++ [ (n, ConstVar n) | (n,_) <- getConstants code ]
        augmentQuantifier bound env pos kind decls body =
          Node (pos, Quantifier kind) [Node (pos, Locals) (augmentDecls bound env (subForest decls)), f ((declNames decls)++bound) env body]
        augmentDecls bound env ds = [ Node (pos, Declaration) [ns, f bound env t] | (Node (pos,Declaration) [ns,t]) <- ds ]
  
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

cvars types (Node (_,Eq) [expr, (Node (p,String c) [])]) =
    case (lookup c types) of
      Nothing -> [(c, Node (p, Const) [typeof types expr])]
      (Just _) -> []

cvars types (Node (_, Conj) [x, y]) = xtypes ++ (cvars (xtypes++types) y)
  where xtypes = cvars types x

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
        f (_, In) [x,y] = "(" ++ x ++ " in " ++ y ++ ")"
        f (_, Conj) [x,y] = x ++ " and " ++ y
        f (_, Disj) [x,y] = "("++ x ++ " or " ++ y++")"
        f (_, Implies) [x,y] = "(" ++ x ++ " => " ++ y ++")"
        f (_, Range) [x,y] = "range[" ++ x ++ "," ++ y ++ "]"
        f (_, Join) xs = (showJoin xs)
        f (_, Product) xs = (showRel xs)
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
        f (_, Quantifier Sum) [decls, e] = "(sum " ++ decls ++ " | " ++ e ++ ")"
        f (_, Quantifier All) [decls, e] = "(all " ++ decls ++ " | " ++ e ++ ")"
        f (_, Quantifier No) [decls, e] = "(no " ++ decls ++ " | " ++ e ++ ")"
        f (_, Quantifier Some) [decls, e] = "(some " ++ decls ++ " | " ++ e ++ ")"
        f (_, SomeSet) [e] = "(some " ++ e ++ ")"
        f (_, String n) [] = n
        f (_, StateVar n) [] = "State." ++ n
        f (_, ConstVar n) [] = "Const." ++ n
        f (_, Pair) [x,y] = "(" ++ x ++ " -> " ++ y  ++ ")"
        f (_, Union) [x,y] = "(" ++ x ++ " + " ++ y  ++ ")"
        f (_, Update) [x,y] = "(" ++ x ++ " ++ " ++ y  ++ ")"
	f (_, Closure) [x] = x
	f other ns = error ("Internal error: Don't know how to show " ++ (show other))

showRel = foldr f ""
  where f x [] = x
	f x xs = "(" ++ x ++ ") -> (" ++ xs ++ ")"

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
     (exitcode, out, err) <- readProcessOutput "java" ["-cp",classpath, "AlloyCmdLine", analysisFile, outputFile] 
     case exitcode of
      ExitSuccess -> putStrLn out
      (ExitFailure _) -> fail err

readProcessOutput :: FilePath -> [String] -> IO (ExitCode,String,String)
readProcessOutput cmd args = do
	(_, Just outh, Just errh, pid) <-
		createProcess (proc cmd args) { std_out = CreatePipe,
						std_err = CreatePipe }
	putLines outh
	err <- hGetContents errh
	ex <- waitForProcess pid
	return (ex, "", err)

putLines :: Handle -> IO ()
putLines outh = do
	result <- try (hGetLine outh)
	case result of
	  Left e -> if isEOFError e then return () else ioError e
	  Right line -> do putStrLn line ; hFlush stdout; putLines outh

report :: Env -> Config -> [(String,Oblig)] -> IO ()

report types cfg obligs = 
  do [outputFile] <- lookupM "alloy.analysisoutput" cfg
     output <- readFile outputFile
     putStrLn ("... Parsing output file: " ++ outputFile)
     checks <- parseOutput output
     putStrLn (showOutput types obligs checks)

showOutput :: Env -> [(String, Oblig)] -> [ (String, Maybe Instance) ] -> String
showOutput types wpEnv checks = foldr (++) "" (map f checks)
  where f (name, Nothing) = ""
        f (name, Just (Instance eqns)) = "\n("++name++") " ++ "When the program starts with:\n\n" ++ (showInst types eqns) ++ "\n" ++ (pathOf name wpEnv) ++ (skolemVars types eqns)

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

showInst :: Env -> [Equation] -> String

showInst types [] = ""
showInst types ((Set _ _):rest) = showInst types rest
showInst types ((Relation kind name tuples):rest) =
  if kind == "thisState"
  then name ++ "=" ++ (showRelation types name (map tail tuples)) ++"\n" ++ (showInst types rest)
  else showInst types rest

skolemVars :: Env -> [ Equation ] -> String

skolemVars types eqs = 
  if skolems == "" 
  then ""
  else "in particular for " ++ skolems
  where skolems = showSkolem types eqs

showSkolem :: Env -> [ Equation ] -> String
showSkolem types = foldr f "" 
  where f (Relation "Local" name tuples) rest = showRelation types name tuples
        f _ rest = rest 

showRelation :: Env -> String -> [Tuple] -> String
showRelation env name tuples = 
  case (lookup name env) of 
     Nothing -> error ("Type error: " ++ name ++ " is untyped.")
     (Just t) -> showTuples t tuples

showTuples :: AST -> [Tuple] -> String
showTuples typ ts = 
  case typ of
    Node (_,Product) es -> "[" ++ (showAsRelation ts) ++ "]"
    Node (_,ArrayType _ _) [] -> "[" ++ (showAsArray ts) ++ "]"
    otherwise -> head (head ts)

{-
a tuple that represents a relation is a list of the form

[[x11,...,x1n],[x21,...,x2n],...,[xm1,...xmn]]

to show it as a relation we first convert each internal list into
a single string of the form

xi1 -> xi2 -> ... -> xin

and then collect them together separated with commas

-}

showAsRelation :: [Tuple] -> String
showAsRelation = separateByComma . (map showIndividualTuple)

showIndividualTuple :: Tuple -> String
showIndividualTuple = foldr f "" 
  where f v [] = v
        f v (t:ts) = v ++ "->" ++ (t:ts)

separateByComma :: [String] -> String
separateByComma = foldr f ""
  where f v [] = v
        f v (t:ts) = v ++ ", " ++ (t:ts)

showAsArray = showArray

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

