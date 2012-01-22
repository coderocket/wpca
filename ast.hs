module AST where
import Data.Tree
import Lexer

type AST = Tree (AlexPosn,Kind)

data Kind = Int Int | String String | Type String | Spec | Locals | Declaration | Assign | Loop | Cond | Seq | Skip | Neg | True | False | Const | Plus| Minus | Times | Quotient | Div | Mod  | Eq | Geq | Leq | Conj | Disj | Implies | Join | Greater | Less | List | Not

foldRose :: (a -> [b] -> b) -> Tree a -> b
foldRose f (Node x ts) = f x (map (foldRose f) ts)

declsToList :: [AST] -> [(String, AST)]

declsToList ds = foldr (++) [] (map f ds) 
  where f (Node (_,Declaration) [(Node (_,List) ds), t]) = foldr (\ (Node (_,String x) []) xs -> (x,t):xs) [] ds

not :: AST -> AST 

not x = Node (fst (rootLabel x), Not) [x]

conj :: AST -> AST -> AST

conj x y = Node (fst (rootLabel x), Conj) [x, y]

disj :: AST -> AST -> AST

disj x y = Node (fst (rootLabel x), Disj) [x, y]

implies :: AST -> AST -> AST

implies x y = Node (fst (rootLabel x), Implies) [x, y]

annotate :: String -> AST -> AST

annotate s x = Node (fst (rootLabel x), String s) [x]

true :: AST
true = Node (alexStartPos, AST.True) []

false :: AST
false = Node (alexStartPos, AST.False) []

