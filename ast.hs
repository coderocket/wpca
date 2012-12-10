module AST where
import Data.Tree
import Loc

type Env = [(String,AST)]

type AST = Tree (Loc,Kind)

data Kind = Int Int | String String | Type String | Spec | Declaration | Assert | Assign | Loop | Cond | Seq | Skip | Alloc String | Neg | True | False | Const | Plus| Minus | Times | Quotient | Div | Mod  | NotEq | Eq | Geq | Leq | Conj | Disj | Implies | Join | ArrayJoin | Greater | Less | List | Not | Break | ArrayType String String | Range | Quantifier Quantifier | Pair | Union | Update | Closure | SomeSet | Product | In | Proc String | Record String | Program | Output | OutputVar | RecordType String | Reverse | SetDiff | SetType String | Call String | SpecStmt | PassByRef String
  deriving (Eq,Show)

data PassStyle = PassIn | PassOut | PassInOut
  deriving (Eq,Show)

data Quantifier = Sum | All | No | Some
  deriving (Eq,Show)

foldRose :: (a -> [b] -> b) -> Tree a -> b
foldRose f (Node x ts) = f x (map (foldRose f) ts)

declsToList :: [AST] -> [(String, AST)]

declsToList ds = foldr (++) [] (map f ds) 
  where f (Node (_,Declaration) [(Node (_,List) ds), t]) = foldr (\ (Node (_,String x) []) xs -> (x,t):xs) [] ds
        f err = error ("declstToList does not apply to " ++ (show err))

separateDecls :: [AST] -> AST
separateDecls = Node (startLoc, List) . foldr f []
  where f (Node (dpos,Declaration) [(Node (_,List) ds), t]) rest = 
          (foldr g [] ds) ++ rest
             where g (Node (pos,String n) []) ds = Node (dpos, Declaration) [ Node(pos,List) [string n], t ] : ds


setType :: String -> AST

setType name = Node (startLoc, SetType name) []

setDiff :: AST -> AST -> AST

setDiff x y = Node (startLoc, SetDiff) [x,y]

someSet :: AST -> AST

someSet x = Node (startLoc, SomeSet) [x]

all :: String -> AST -> AST -> AST

all name typ pred = Node (startLoc, Quantifier All) [ list (declaration name typ), pred]

declaration :: String -> AST -> AST

declaration name typ = Node (startLoc, Declaration) [list (string name), typ]

append :: AST -> AST -> AST
append (Node (p1,List) xs) (Node (p2,List) ys) = Node (p1,List) (xs++ys)
append x y = error $ "can't append: " ++ (show x) ++ " to " ++ (show y)

close :: AST -> AST

close x = Node (fst (rootLabel x), Closure) [x]

product :: AST -> AST -> AST 

product x y = Node (fst (rootLabel x), Product) [x,y]

union :: AST -> AST -> AST

union x y = Node (fst (rootLabel x), Union) [x,y]

update :: AST -> AST -> AST

update x y = Node (fst (rootLabel x), Update) [x,y]

pair :: AST -> AST -> AST

pair x y = Node (fst (rootLabel x), Pair) [x,y]

not :: AST -> AST 

not x = Node (fst (rootLabel x), Not) [x]

conj :: AST -> AST -> AST

conj x y = Node (fst (rootLabel x), Conj) [x, y]

disj :: AST -> AST -> AST

disj x y = Node (fst (rootLabel x), Disj) [x, y]

implies :: AST -> AST -> AST

implies x y = Node (fst (rootLabel x), Implies) [x, y]

true :: AST
true = Node (startLoc, AST.True) []

false :: AST
false = Node (startLoc, AST.False) []

break :: AST
break = Node (startLoc, Break) []

skip :: AST
skip = Node (startLoc, Skip) []

wseq :: AST -> AST -> AST
wseq x y = Node (fst (rootLabel x), Seq) [x,y]

assign :: String -> AST -> AST
assign n e = Node (fst (rootLabel e), Assign) [(string n), e]

list :: AST -> AST
list e = Node (fst (rootLabel e), List) [e]

string :: String -> AST
string s = Node (startLoc, String s) []

makeProcEnv :: [AST] -> Env
makeProcEnv procs = [ (getProcName p, p) | p <- procs ]

getProcName :: AST -> String
getProcName (Node (_,Proc name) _) = name

