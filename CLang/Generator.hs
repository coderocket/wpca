module CLang.Generator where
import List
import AST
import Data.Tree
import LookupMonad

showCCode :: [(String,[String])] -> AST -> IO ()

showCCode env ast =
  do cs <- lookupM "c.sourcefile" env
     hs <- lookupM "c.headerfile" env
     fn <- lookupM "c.function" env
     putStrLn ("writing c header file to " ++ (head hs))
     writeFile (head hs) (showCHeader (head fn) ast)
     putStrLn ("writing c source file to " ++ (head cs))
     writeFile (head cs) (showCSource (head fn) ast)

showCHeader name (Node (_,Spec) (locals:_)) =
	"void " ++ name ++ "(" ++ (showLocals locals) ++ ");\n"

showCSource name (Node (_,Spec) [locals, pre, program, post]) = 
	"void " ++ name ++ "(" ++ (showLocals locals) ++ ")\n{\n" 
	++ (showNewLocals locals) ++ "\n"
	++ (showC env (transform program)) ++ "\n}\n"
  where env = declsToList (subForest locals)

showC :: Env -> AST -> String

showC env = foldRose f 
  where f (_,Seq) [x, y] = x ++ "\n" ++ y 
        f (_,Skip) [] = ";"
        f (_,Break) [] = "break;"
        f (_,Type n) [] = n
        f (_,String n) [] = case lookup n env of 
          (Just _) -> "*"++ n
          Nothing -> n
        f (_,Int x) [] = (show x)
        f (_,Neg) [x] = "-" ++ x
        f (_, Plus) [x,y] = "(" ++ x ++ "+" ++ y ++ ")"
        f (_, Minus) [x,y] = "(" ++ x ++ "-" ++ y ++ ")"
        f (_, Times) [x,y] = x ++ "*" ++ y 
        f (_, Quotient) [x,y] = x ++ " / " ++ y
        f (_, Div) [x,y] = x ++ " / " ++ y 
        f (_, Mod) [x,y] = x ++ " % " ++ y
        f (_, Eq) [x,y] = x ++ " == " ++ y
        f (_, Greater) [x,y] = x ++ " > " ++ y
        f (_, Less) [x,y] = x ++ " < " ++ y
        f (_, Geq) [x,y] = x ++ " >= " ++ y
        f (_, Leq) [x,y] = x ++ " <= " ++ y
        f (_, Conj) [x,y] = x ++ " && " ++ y
        f (_, Disj) [x,y] = "("++ x ++ " || " ++ y ++ ")"
        f (_, Implies) [x,y] = "(!(" ++ x ++ ") || " ++ y ++")"
        f (_, AST.True) [] = "1"
        f (_, AST.False) [] = "0"
        f (_,Not) [x] = "!(" ++ x ++ ")"
        f (_,Assign) [n,e] = n ++ " = " ++ e ++ ";"
        f (_,Cond) [g,x,y] = "if (" ++ g ++ ") {\n" ++ x ++ "} else {\n" ++ y ++"}\n"
        f (_,Loop) [gs] = "while(1) {\n" ++ gs ++ "\n}\n"
        f (_,x) xs = show x

showLocals (Node (_,Locals) ds) = foldr f "" (declsToList ds) 
  where f (n,t) [] = (showC [] t) ++ " *" ++ n
        f (n,t) ds = (showC [] t) ++ " *" ++ n ++ ", " ++ ds

showNewLocals (Node (_,Locals) ds) = foldr f "" (declsToList ds) 
  where f (n,t) xs = (showC [] t) ++ " " ++ n ++ "_new;" ++ xs
        
transform :: AST -> AST
transform = foldRose f
  where f (_,Assign) [Node (_,List) ns, Node (_,List) es] = transformAssign ns es
        f (_,Cond) gs = transformCond gs
        f (pos,Loop) [_,(Node (_,List) gs)] = Node (pos, Loop) [transformLoop gs]
        f (pos,kind) xs = Node (pos,kind) xs

transformAssign ns es = (assignNew (zip ns es)) `wseq` (assignVars (map h ns))
  where h (Node (_,String s) []) = s

transformCond = foldr f skip
  where f (Node (pos,List) [g,s]) gs = Node (pos,Cond) [g,s,gs] 

transformLoop = foldr f AST.break
  where f (Node (pos,List) [g,s]) gs = Node (pos,Cond) [g,s,gs]

assignNew :: [(AST,AST)] -> AST
assignNew = foldr f skip
  where f (Node (_,String n)[], e) as = (assign (n++"_new") e) `wseq` as

assignVars :: [String] -> AST
assignVars = foldr f skip
  where f n as = assign n (string (n++"_new")) `wseq` as

