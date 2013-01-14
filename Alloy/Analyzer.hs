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
import Alloy.Show

{- 

A WPCA program consists of a set of record definitions and a set of
procedure definitions. The record definitions are global to all the
procedures but the analysis of each procedure is independent of the
other procedures. Therefore, to analyze a WPCA program we analyze 
each procedure independently, passing it the global set of record
definitions. Now that we support calling procedures we must also pass
an environment of procedures to the analysis procedure. In addition,
we must pre-process the procedures to ensure that we can use their
specifications when calculating the obligations of a procedure call.

-}

work :: Config -> AST -> IO ()
work config (Node (_,Program) [Node (_,List) records, Node (_,List) procs, Node (_,List) theory]) = analyze config records (preProcess records procs) theory

preProcess :: [AST] -> [AST] -> [AST]

preProcess records procs = [ preProcessProc records $ extractConstants records p | p <- procs ]

preProcessProc records proc = tidyOps types $ tidyJoins proc
  where types = (getRecordTypes records) ++ (getStateVars records proc)

extractConstants :: [AST] -> AST -> AST

extractConstants records (Node (p, Proc name) [params, locals, pre, program, post]) = Node (p, Proc name) [params, locals, constants, pre, program, post]
  where constants = Node (startLoc, List) (cvarsX (getStateEnv records (subForest params)) pre)

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

cvarsX :: Env -> AST -> [AST]

cvarsX types (Node (_,Eq) [expr, (Node (p,String c) [])]) =
    case (lookup c types) of
      Nothing -> [ Node (startLoc, Declaration) [ Node (startLoc, List) [string c,expr], typeof types expr] ]
      (Just _) -> []

cvarsX types (Node (_, Conj) [x, y]) = xtypes ++ (cvarsX ((declsToList xtypes)++types) y)
  where xtypes = cvarsX types x

cvarsX types _ = []

getStateVars :: [AST] -> AST -> Env

getStateVars records (Node (_,Proc _) [params, locals, constants, pre, program, post]) = 
  builtInStateVars ++ getStateEnv records (subForest (append params locals))
  where builtInStateVars = [("extent", setType "Object")]

analyze :: Config -> [AST] -> [AST] -> [AST] -> IO ()
analyze config records procs theory = loop procs 
  where loop [] = return ()
        loop (p:ps) = do analyzeProc config env theory records p 
                         loop ps
        env = makeProcEnv procs

{-

To analyse a wpca procedure we first generate an alloy model from
the code, then we run the alloy analyzer on the model and finally
we generate a report based on the results of the analysis.

-}

analyzeProc :: Config -> Env -> [AST] -> [AST]-> AST -> IO ()

analyzeProc cfg procs theory records proc =
  do (obligs,types) <- generate procConfig procs theory records proc
     analyse procConfig 
     report types procConfig obligs
  where procConfig = makeFileNames (getProcName proc) cfg

makeFileNames :: String -> Config -> Config

makeFileNames name config = 
  do [analysisStem] <- lookupM "alloy.analysisfile" config 
     [outputStem] <- lookupM "alloy.analysisoutput" config
     (("alloy.analysisfile", [name ++ "." ++ analysisStem]):("alloy.analysisoutput", [name ++ "." ++ outputStem]):config) 

{- To generate the model we follow these steps:

1. extract the state variables, the constants and the global constraints
2. calculate the proof obligations 
3. Retrieve the name of the analysis file from the configuration and
   write the model to the analysis file.

Note that we return the proof obligations because we need them to generate
the report.

-}

generate :: Config -> Env -> [AST] -> [AST] -> AST -> IO ([(String,Oblig)], Env)

generate cfg procs theory records proc = 
  do putStrLn $ "Generating model for procedure " ++ (getProcName proc) ++ "..." 
     stateVars <- return $ getStateVars records proc
     constants <- return $ getConstants proc 
     obligs <- calcObligs procs proc
     consistencyCheck <- generateConsistencyCheck proc
     [analysisFile] <- lookupM "alloy.analysisfile" cfg
     libraries <- lookupM "alloy.analysislibraries" cfg
     putStrLn ("... There are " ++ (show (length obligs)) ++ " proof obligations. Writing analysis file to " ++ analysisFile)
     theoryText <- readTheories [s | Node (_,String s) [] <- theory]
     writeFile analysisFile (showModel libraries theoryText records stateVars constants obligs)
     return (obligs, stateVars ++ constants)

generateConsistencyCheck :: AST -> IO String
generateConsistencyCheck (Node (p,Proc _) [params,locals,constants,pre,body,post]) = return ""

-- Alloy does not care if the join is due to an array or due to a relation

tidyJoins :: AST -> AST
tidyJoins = foldRose f 
  where f (pos, ArrayJoin) es = Node (pos, Join) es
        f k es = Node k es

-- Decide if to treat the two operands of a '+' or a '-' operator as sets or as integers.
-- We treat both as sets unless both types are integers.

tidyOps :: Env -> AST -> AST
tidyOps env (Node (pos, Plus) [x,y]) = 
  if typeof env x `sameTypeAs` intType && typeof env y `sameTypeAs` intType
  then Node (pos, Plus) [x,y]
  else Node (pos, Union) [x,y]
tidyOps env (Node (pos, Minus) [x,y]) = 
  if typeof env x `sameTypeAs` intType && typeof env y `sameTypeAs` intType
  then Node (pos, Minus) [x,y]
  else Node (pos, SetDiff) [x,y]

tidyOps env (Node (pos, Quantifier q) [ Node(_,List) decls, body]) = 
  Node (pos, Quantifier q) [tidyOpsDecls env decls, tidyOps ((declsToList decls) ++ env) body]
tidyOps env (Node (pos, t) subnodes) = 
  Node (pos, t) (map (tidyOps env) subnodes) 

tidyOpsDecls :: Env -> [AST] -> AST

tidyOpsDecls env decls = Node (startLoc, List) [ Node (startLoc, Declaration) [ns, tidyOps env t] | (Node (_,Declaration) [ns,t]) <- decls ]

getConstants :: AST -> Env

getConstants (Node (_,Proc _) [params, locals, Node (_,List) constants, pre, program, post]) = 
  map f constants 
  where f (Node (_,Declaration) [(Node (_,List) [Node (_,String x) [],e]), t]) = (x,t)

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

calcObligs procs (Node (_,Proc _) [params, locals, constants, pre, program, post]) = 
 return $ zip names [ (pre `implies` wp,path,goal) | (wp,path,goal) <- obligations]
 where names = ["test"++(show i) | i <- [1..] ] 
       obligations = wpx procs program [(post,[],"satisfy the postcondition\n\n" ++ (show (fst (npos post))) ++ ": " ++ (showA post))]

readTheories :: [String] -> IO String
readTheories [] = return ""
readTheories (theory : theories) = 
	do first <- readFile (theory ++ ".als")
           rest <- readTheories theories
           return (first++ "\n" ++ rest)

showModel :: [String] -> String -> [AST] -> Env -> Env -> [(String,Oblig)] -> String

showModel libraries theory records stateVars constants obligs =
 (foldr (++) "" [ "open " ++ s ++ "\n" | s <- libraries])
 ++ "\n" ++ theory ++ "\n"
 ++ (showRecords records) 
 ++ (showStateSig stateVars)
 ++ (foldr (++) "" (map (showOblig (stateVars ++ constants)) obligs))

showRecords :: [AST] -> String
showRecords = foldr (++) [] . map showRecord

showStateSig :: Env -> String

showStateSig vars = header ++ fields ++ footer
  where header = "sig STATE {\n"
        fields = separateByComma (map showStateVar vars)
        footer = "}\n"

showStateVar :: (String, AST) -> String
showStateVar (name,t) = "STATE_" ++ name ++ ": " ++ (showType t) ++ "\n"

showRecord :: AST -> String
showRecord (Node (_,Record name) defs) = header ++ fields ++ footer
  where header = "sig " ++ name ++ " extends Object {\n" 
        fields = "" -- separateByComma (map showDef defs)
        footer = "}\n"

showDef :: AST -> String
showDef (Node (_,Declaration) [Node (_,List) names,typ]) = (showNames [ n | (Node (_,String n) []) <- names]) ++ ":" ++ showType typ ++ "\n"

showConstraints :: Env -> String
showConstraints env = "{" ++ (foldr (++) "" (map f env)) ++ "}"
 where
 f (n, Node (_, Type "nat") []) = "(" ++ n++".gte[0])\n"
 f (n, Node (_, ArrayType k _) []) = "(#"++n++"="++k++")\n"
 f (n, Node (_, Output) [t]) = f (n,t)
 f _ = ""

showOblig env (nm, (wp, path, goal)) = comment ++ pred ++ assertion ++ check
  where comment = "\n/*\ngoal: " ++ goal ++ "\npath: " ++ (show path) ++ "\n*/\n" 
        pred = "pred pred_"  ++ nm ++ "[" ++ (showEnv env) ++ "]\n{\n" ++ (showConstraints env) ++ " =>\n\t" ++ (showA wp) ++ "\n}\n"
        assertion = "assert " ++ nm ++ " {\nall " ++ (showEnv env) ++ "| pred_" ++ nm ++ "[" ++ (showEnvNames env) ++ "]" ++ "\n}\n"
        check = "check " ++ nm ++ "\n\n" 
-- " for 3 but 2 Int\n\n"

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
        f (_, String n) [] = n ++ " + NIL"
        f (_, Const) [x] = x
        f (_, Product) xs = (showRel xs)
	f (_, Output) [x] = x
	f other ns = error ("Internal error: Don't know how to show the type " ++ (show other)) 

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
        f (name, Just (Instance eqns)) = "\n("++name++") " ++ "error:\n\n" ++ (showSkolem types eqns) ++ "\n" ++ (pathOf name wpEnv) 

pathOf :: String -> [(String, Oblig)] -> String

pathOf name env = case (lookup name env) of
  Nothing -> "... Oops, there is no information about this check :("
  (Just (_,[],goal)) -> "fails to " ++ goal ++ "\n"
  (Just (_,path,goal)) -> "satisfies\n\n" ++ (showPath path) ++ "\n\n" 
			++ "but fails to " ++ goal ++ "\n"

showPath [] = ""
showPath [((line,col),text)] = (show line) ++ ": " ++ text
showPath (((line,col),text):(r:rest)) = (show line) ++ ": " ++ text ++ "\n" ++ (showPath (r:rest))

showInst :: Env -> [Equation] -> String

showInst types [] = ""
showInst types ((Set _ _):rest) = showInst types rest
showInst types ((Relation kind name tuples):rest) =
  if kind == "thisState"
  then name ++ "=" ++ (showRelation types name (map tail tuples)) ++"\n" ++ (showInst types rest)
  else showInst types rest

showSkolem :: Env -> [ Equation ] -> String
showSkolem types = foldr f "" 
  where f (Relation "Local" sname tuples) rest = 
		name ++ " = " ++ (showRelation types name tuples) ++ "\n" ++ rest
		where name = (removeSkolemPrefix sname)
        f _ rest = rest 

{- skolem variables have a prefix of the form '$'<test name>'_' that we
have to remove or we won't find the variable in the type environment.
-}
 
removeSkolemPrefix :: String -> String
removeSkolemPrefix = drop 1 . dropWhile (/= '_')

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
    Node (_,Output) [x] -> showTuples x ts
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

