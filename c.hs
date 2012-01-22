module CLang where
import List
import AST
import WPC

showCCode :: [(String,[String])] -> AST -> Maybe (IO ())

showCCode env ast =
  do cs <- lookup "c.sourcefile" env
     hs <- lookup "c.headerfile" env
     fn <- lookup "c.function" env
     return $ do putStrLn ("writing c header file to " ++ (head hs))
{-
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
showC (Loop _ gs) = "while(1) {\n" ++ (showLoop gs) ++ "\n}\n"
showC (Seq x y) = (showC x) ++ "\n" ++ (showC y)
showC Skip = ";"
showC (TypeVar vn) = vn
showC (Var vn) = "*"++vn
showC (Nat x) = (show x)
showC (Neg x) = "-" ++ (showC x)
showC (BinOp Plus x y) = "(" ++ (showC x) ++ "+" ++ (showC y) ++ ")"
showC (BinOp Minus x y) = "(" ++ (showC x) ++ "-" ++ (showC y) ++ ")"
showC (BinOp Times x y) = (showC x) ++ "*" ++ (showC y) 
showC (BinOp Quotient x y) = (showC x) ++ " / " ++ (showC y)
showC (BinOp Div x y) = (showC x) ++ " / " ++ (showC y) 
showC (BinOp Mod x y) = (showC x) ++ " % " ++ (showC y)
showC (BinOp NodeEq x y) = (showC x) ++ " == " ++ (showC y)
showC (BinOp NodeGreater x y) = (showC x) ++ " > " ++ (showC y)
showC (BinOp NodeLess x y) = (showC x) ++ " < " ++ (showC y)
showC (BinOp NodeGeq x y) = (showC x) ++ " >= " ++ (showC y)
showC (BinOp NodeLeq x y) = (showC x) ++ " <= " ++ (showC y)
showC (BinOp Conj x y) = (showC x) ++ " && " ++ (showC y)
showC (BinOp Disj x y) = "("++ (showC x) ++ " || " ++ (showC y) ++ ")"
showC (BinOp Implies x y) = "(!(" ++ (showC x) ++ ") || " ++ (showC y) ++")"
showC PredTrue = "1"
showC PredFalse = "0"
showC (Not x) = "!(" ++ (showC x) ++ ")"

showDecls ds = foldr f "" (declsToList ds)
  where f (n,t) [] = (showC t) ++ " *" ++ n
        f (n,t) ds = (showC t) ++ " *" ++ n ++ ", " ++ ds

showNewLocals (Locals ds) = foldr f "" (declsToList ds) 
  where f (n,t) xs = (showC t) ++ " " ++ n ++ "_new;" ++ xs

showAssign aa = (assignNew aa) ++ (assignVars aa) 
assignNew = foldr f "" where f (n,e) s = n++"_new = "++(showC e)++";\n"++s 
assignVars = foldr f "" where f (n,_) s = "*"++n++" = " ++ n++"_new" ++";\n" ++ s

showCond = foldr f "" 
  where f (g,s) gs = "if (" ++ (showC g) ++ ") {\n" ++ (showC s) ++ "} else {\n" ++ gs ++"}\n"

showLoop = foldr f "" 
  where f (g,s) "" = "if (" ++ (showC g) ++ ") {\n" ++ (showC s) ++ "} else break;" 
        f (g,s) gs = "if (" ++ (showC g) ++ ") {\n" ++ (showC s) ++ "} else {\n" ++ gs ++ "}\n"
-}
