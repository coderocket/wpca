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
import Loc

{- 

A WPCA program consists of a set of record definitions and a set of
procedure definitions. The record definitions are global to all the
procedures but the analysis of each procedure is independent of the
other procedures. Therefore, to analyze a WPCA program we analyze 
each procedure independently, passing it the global set of record
definitions.

-}

work :: Config -> AST -> IO ()
work cfg (Node (_,Program) [Node (_,List) records, Node (_,List) procs, Node (_,List) theory]) = 
  loop eprocs
  where loop [] = return ()
        loop (p:ps) = do analyzeProc cfg env theory records p 
                         loop ps
	eprocs = extractConstants records procs
	env = [ (getProcName proc, proc) | proc <- eprocs ]

getProcName (Node (_,Proc name) _) = name

extractConstants :: [AST] -> [AST] -> [AST]

extractConstants records procs = 
  [ Node (p, Proc name) [params, locals, f params pre, pre, body, post] | 
           (Node (p, Proc name) [params, locals, pre, body, post]) <- procs ] 
  where f params pre = Node (startLoc, List) (cvars (getStateEnv records (subForest params)) pre)

cvars :: Env -> AST -> [AST]

-- To find the type of a constant (a.k.a model) variable
-- we look for either a top level equality expression or
-- an equality expression in top level conjunctions where
-- the left hand side is a state variable and the right
-- hand side is the constant variable.

cvars types (Node (_,Eq) [expr, (Node (p,String c) [])]) =
    case (lookup c types) of
      Nothing -> [ declaration c (typeof types expr) ]
      (Just _) -> []

cvars types (Node (_, Conj) [x, y]) = xtypes ++ (cvars ((declsToList xtypes)++types) y)
  where xtypes = cvars types x

cvars types _ = []

analyzeProc :: Config -> Env -> [AST] -> [AST]-> AST -> IO ()
analyzeProc cfg procs theory records (Node (p,Proc name) [params,locals,constants,pre,body,post]) = 
  do [analysisFile] <- lookupM "alloy.analysisfile" cfg 
     analyzeSpec (("alloy.analysisfile", [name ++ "." ++ analysisFile]):cfg) procs theory records (Node (p, Spec) [append params locals, constants, pre,body,post])

{-

To analyse a wpca procedure we first generate an alloy model from
the code, then we run the alloy analyzer on the model and finally
we generate a report based on the results of the analysis.

-}

analyzeSpec :: Config -> Env -> [AST] -> [AST] -> AST -> IO ()

analyzeSpec cfg procs theory records code = 
  do (obligs,types) <- generate cfg procs theory records code
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

generate :: Config -> Env -> [AST] -> [AST] -> AST -> IO ([(String,Oblig)], Env)

generate cfg procs theory records code = 
  do putStrLn "Generating model..." 
     stateVars <- return $ getStateVars records code
     constants <- return $ getConstants code 
     acode <- augment records (tidyOps ((getRecordTypes records) ++ stateVars) (tidyJoins code))
     obligs <- calcObligs procs acode
     consistencyCheck <- generateConsistencyCheck acode
     [analysisFile] <- lookupM "alloy.analysisfile" cfg
     libraries <- lookupM "alloy.analysislibraries" cfg
     putStrLn ("... There are " ++ (show (length obligs)) ++ " proof obligations. Writing analysis file to " ++ analysisFile)
     writeFile analysisFile (showModel (libraries++[s | Node (_,String s) [] <- theory]) records stateVars constants obligs)
     return (obligs, stateVars ++ constants)

generateConsistencyCheck :: AST -> IO String
generateConsistencyCheck (Node (p,Spec) [locals,constants,pre,body,post]) = return ""

-- Alloy does not care if the join is due to an array or due to a relation

tidyJoins :: AST -> AST
tidyJoins = foldRose f 
  where f (pos, ArrayJoin) es = Node (pos, Join) es
        f k es = Node k es

-- Decide if to treat the two operands of a '+' or a '-' operator as sets or as integers.
-- We treat both as sets unless both types are integers.

tidyOps :: Env -> AST -> AST
tidyOps env (Node (pos, Plus) [x,y]) = 
  if typeof env x == intType && typeof env y == intType
  then Node (pos, Plus) [x,y]
  else Node (pos, Union) [x,y]
tidyOps env (Node (pos, Minus) [x,y]) = 
  if typeof env x == intType && typeof env y == intType
  then Node (pos, Minus) [x,y]
  else Node (pos, SetDiff) [x,y]
tidyOps env (Node (pos, Quantifier q) [ Node(_,List) decls, body]) = 
  Node (pos, Quantifier q) [tidyOpsDecls env decls, tidyOps ((declsToList decls) ++ env) body]
tidyOps env (Node (pos, t) subnodes) = 
  Node (pos, t) (map (tidyOps env) subnodes) 

tidyOpsDecls :: Env -> [AST] -> AST

tidyOpsDecls env decls = Node (startLoc, List) [ Node (startLoc, Declaration) [ns, tidyOps env t] | (Node (_,Declaration) [ns,t]) <- decls ]


augment :: [AST] -> AST -> IO (AST)

augment records code@(Node (pos,Spec) [locals, constants, pre, program, post]) = 
  return $ Node (pos,Spec) [locals, constants, f [] initEnv pre, f [] initEnv program, f [] initEnv post] 
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
        initEnv = [ (n, String n) | (n,_) <- (getStateVars records code) ] ++ [ (n, String n) | (n,_) <- getConstants code ]
        augmentQuantifier bound env pos kind decls body =
          Node (pos, Quantifier kind) [Node (pos, List) (augmentDecls bound env (subForest decls)), f ((declNames decls)++bound) env body]
        augmentDecls bound env ds = [ Node (pos, Declaration) [ns, f bound env t] | (Node (pos,Declaration) [ns,t]) <- ds ]
  
getConstants :: AST -> Env

getConstants (Node (_,Spec) [locals, Node (_,List) constants, pre, program, post]) = 
	declsToList constants

getStateVars :: [AST] -> AST -> Env

getStateVars records (Node (_,Spec) [locals, constants, pre, program, post]) = 
  builtInStateVars ++ getStateEnv records (subForest locals)
  where builtInStateVars = [("extent", setType "Object")]

getStateEnv :: [AST] -> [AST] -> Env
getStateEnv records locals = 
  (getRecordVars records) ++ (declsToList locals)

getRecordVars :: [AST] -> Env
getRecordVars = foldr (++) [] . map getFieldVars

getFieldVars :: AST -> Env
getFieldVars (Node (_,Record name) fields) = f fields 
  where f = map (getFieldVar name) . declsToList 

getFieldVar :: String -> (String,AST) -> (String,AST)
getFieldVar left (relName, right) = 
  (relName, Node (startLoc,Product) [ Node (startLoc,String left) [], right])

getRecordTypes :: [AST] -> Env
getRecordTypes = map getRecordType

getRecordType :: AST -> (String,AST)
getRecordType (Node (_, Record name) _) = (name, setType name)

calcObligs procs (Node (_,Spec) [locals, constants, pre, program, post]) = 
 return $ zip names [ (pre `implies` wp,path,goal) | (wp,path,goal) <- obligations]
 where names = ["test"++(show i) | i <- [1..] ] 
       obligations = wpx procs program [(post,[],"satisfy the postcondition")]

showModel :: [String] -> [AST] -> Env -> Env -> [(String,Oblig)] -> String

showModel libraries records stateVars constants obligs =
 (foldr (++) "" [ "open " ++ s ++ "\n" | s <- libraries])
 ++ (showRecords records) 
 ++ (foldr (++) "" (map (showOblig (stateVars ++ constants)) obligs))

showRecords :: [AST] -> String
showRecords = foldr (++) [] . map showRecord

showRecord :: AST -> String
showRecord (Node (_,Record name) defs) = header ++ fields ++ footer
  where header = "sig " ++ name ++ " extends Object {\n" 
        fields = separateByComma (map showDef defs)
        footer = "}\n"

showDef :: AST -> String
showDef (Node (_,Declaration) [Node (_,List) names,typ]) = (showNames [ n | (Node (_,String n) []) <- names]) ++ ":" ++ showType typ ++ "\n"

showConstraints :: Env -> String
showConstraints env = "{" ++ (foldr (++) "" (map f env)) ++ "}"
 where
 f (n, Node (_, Type "nat") []) = "(" ++ n++".gte[0])\n"
 f (n, Node (_, ArrayType k _) []) = "(#"++n++"="++k++")\n"
 f _ = ""

showOblig env (nm, (wp, path, goal)) = comment ++ pred ++ assertion ++ check
  where comment = "\n/*\ngoal: " ++ goal ++ "\npath: " ++ (show path) ++ "\n*/\n" 
        pred = "pred pred_"  ++ nm ++ "[" ++ (showEnv env) ++ "]\n{\n" ++ (showConstraints env) ++ " =>\n\t" ++ (showA wp) ++ "\n}\n"
        assertion = "assert " ++ nm ++ " {\nall " ++ (showEnv env) ++ "| pred_" ++ nm ++ "[" ++ (showEnvNames env) ++ "]" ++ "\n}\n"
        check = "check " ++ nm ++ "\n\n"

showEnv :: Env -> String
showEnv [] = ""
showEnv [(s,n)] = showBinding (s,n)
showEnv ((s,n):ns) = showBinding (s,n) ++ ",\n" ++ (showEnv ns)

showBinding :: (String,AST) -> String
showBinding (name, x) = name ++ " : " ++ (showType x)

showEnvNames :: Env -> String
showEnvNames = showNames . fst . unzip 

showType :: AST -> String
showType = foldRose f
  where f (_, Type "int") [] = "Int"
        f (_, Type "nat") [] = "Int"
        f (_, Type n) [] = n
        f (_, ArrayType _ t) [] = if t == "int" then "seq Int" else ("seq " ++ t)
        f (_, SetType t) [] = "set " ++ t
        f (_, String n) [] = n ++ " + NULL"
        f (_, Const) [x] = x
        f (_, Product) xs = (showRel xs)
	f (_, Output) [x] = x
	f other ns = error ("Internal error: Don't know how to show the type " ++ (show other)) 

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
        f (_, Reverse) [x] = "~(" ++ x ++ ")"
        f (_, Int x) [] = show x
        f (_, AST.True) [] = "true"
        f (_, AST.False) [] = "false"
        f (_, Const) [x] = x
        f (_, Type "int") [] = "Int"
        f (_, Type "nat") [] = "Int"
        f (_, Type n) [] = n
	f (_, Output) [x] = x
        f (_, ArrayType _ t) [] = if t == "int" then "seq Int" else ("seq " ++ t)
        f (_, Declaration) [names, typ] = names ++ " : " ++ typ
        f (_, List) names = showNames names
        f (_, Quantifier Sum) [decls, e] = "(sum " ++ decls ++ " | " ++ e ++ ")"
        f (_, Quantifier All) [decls, e] = "(all " ++ decls ++ " | " ++ e ++ ")"
        f (_, Quantifier No) [decls, e] = "(no " ++ decls ++ " | " ++ e ++ ")"
        f (_, Quantifier Some) [decls, e] = "(some " ++ decls ++ " | " ++ e ++ ")"
        f (_, SomeSet) [e] = "(some " ++ e ++ ")"
        f (_, String n) [] = n
        f (_, Pair) [x,y] = "(" ++ x ++ " -> " ++ y  ++ ")"
        f (_, Union) [x,y] = "(" ++ x ++ " + " ++ y  ++ ")"
        f (_, SetDiff) [x,y] = "(" ++ x ++ " - " ++ y  ++ ")"
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
    Node (_,SetType _) [] -> "[" ++ (showAsRelation ts) ++ "]"
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

