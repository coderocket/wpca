module CLang where
import List
import AST
import WPC

showCCode :: [(String,[String])] -> Node -> Maybe (IO ())

showCCode env ast =
  do cs <- lookup "c.sourcefile" env
     hs <- lookup "c.headerfile" env
     fn <- lookup "c.function" env
     return $ do putStrLn ("writing c header file to " ++ (head hs))
                 writeFile (head hs) (showCHeader (head fn) ast)
                 putStrLn ("writing c source file to " ++ (head cs))
                 writeFile (head cs) (showCSource (head fn) ast)

showCHeader name (Spec locals _ _ _) =
	"void " ++ name ++ "(" ++ (showC locals) ++ ");\n"

showCSource name (Spec locals pre program post) = 
	"void " ++ name ++ "(" ++ (showC locals) ++ ")\n{\n" 
	++ (showNewLocals locals) ++ "\n"
	++ (showC program)
	++ "\n}\n"

showC (Locals ds) = showDecls ds 
showC (Assign (ns,es)) = showAssign (zip ns es)
showC (Cond gs) = showCond gs
--showC (Loop gs) = showLoop gs
showC (Seq x y) = (showC x) ++ "\n" ++ (showC y)
showC Skip = ";"
showC (Plus x y) = "(" ++ (showC x) ++ "+" ++ (showC y) ++ ")"
showC (Minus x y) = "(" ++ (showC x) ++ "-" ++ (showC y) ++ ")"
showC (Times x y) = (showC x) ++ "*" ++ (showC y) 
showC (TypeVar vn) = vn
showC (Var vn) = "*"++vn
showC (Nat x) = (show x)
showC (Neg x) = "-" ++ (showC x)
showC (Quotient x y) = (showC x) ++ " / " ++ (showC y)
showC (Div x y) = (showC x) ++ " / " ++ (showC y) 
showC (Mod x y) = (showC x) ++ " % " ++ (showC y)
showC (NodeEq x y) = (showC x) ++ " == " ++ (showC y)
showC (NodeGeq x y) = (showC x) ++ " >= " ++ (showC y)
showC (NodeLeq x y) = (showC x) ++ " <= " ++ (showC y)
showC (Conj x y) = (showC x) ++ " && " ++ (showC y)
showC (Disj x y) = "("++ (showC x) ++ " || " ++ (showC y) ++ ")"
showC (Implies x y) = "(!(" ++ (showC x) ++ ") || " ++ (showC y) ++")"
showC PredTrue = "1"
showC PredFalse = "0"

showDecls ds = foldr f "" (declsToList ds)
  where f (n,t) [] = (showC t) ++ " *" ++ n
        f (n,t) ds = (showC t) ++ " *" ++ n ++ ", " ++ ds

showNewLocals (Locals ds) = foldr f "" (declsToList ds) 
  where f (n,t) xs = (showC t) ++ " " ++ n ++ "_new;" ++ xs

showAssign aa = (assignNew aa) ++ (assignVars aa) 
assignNew = foldr f "" where f (n,e) s = n++"_new = "++(showC e)++";\n"++s 
assignVars = foldr f "" where f (n,_) s = "*"++n++" = " ++ n++"_new" ++";\n" ++ s

showCond = foldr f "" where f (g,s) gs = "if (" ++ (showC g) ++ ") {\n" ++ (showC s) ++ "} else {\n" ++ gs ++"}\n"
