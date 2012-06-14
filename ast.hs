module AST where
import Data.Tree
import Loc

type Env = [(String,AST)]

type AST = Tree (Loc,Kind)

data Kind = Int Int | String String | Type String | Spec | Locals | Declaration | Assert | Assign | Loop | Cond | Seq | Skip | Neg | True | False | Const | Plus| Minus | Times | Quotient | Div | Mod  | NotEq | Eq | Geq | Leq | Conj | Disj | Implies | Join | Greater | Less | List | Not | Break | ArrayType String String | Range | Quantifier Quantifier | StateVar String | ConstVar String | Pair | Union | Update | Closure 
  deriving (Show)

data Quantifier = Sum | All | No | Some
  deriving (Show)

foldRose :: (a -> [b] -> b) -> Tree a -> b
foldRose f (Node x ts) = f x (map (foldRose f) ts)

declsToList :: [AST] -> [(String, AST)]

declsToList ds = foldr (++) [] (map f ds) 
  where f (Node (_,Declaration) [(Node (_,List) ds), t]) = foldr (\ (Node (_,String x) []) xs -> (x,t):xs) [] ds

close :: AST -> AST

close x = Node (fst (rootLabel x), Closure) [x]

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

stateVar :: String -> AST
stateVar s = Node (startLoc, StateVar s) []

