module WPC where
import List
import AST

wp :: Node -> Node -> Node

wp (Assign (ns,es)) post = subst post (zip ns es)
wp (Cond gs) post = Conj (altguards gs) (wpcond post gs)
-- wp (Loop gs) post =
wp (Seq x y) post = wp x ((wp y) post)
wp Skip post = post

altguards [(g,s)] = g
altguards ((g,s):gs) = Disj g (altguards gs)

wpcond post [(g,s)] = Implies g (wp s post) 
wpcond post ((g,s):gs) = Conj (Implies g (wp s post)) (wpcond post gs)

subst :: Node -> [(String,Node)] -> Node

subst (Var vn) ne = 
  case (lookup vn ne) of 
	(Just e) -> e
	Nothing -> (Var vn)
subst (TypeVar vn) ne = TypeVar vn
subst (Plus x y) ne = Plus (subst x ne) (subst y ne)
subst (Minus x y) ne = Minus (subst x ne) (subst y ne)
subst (Times x y) ne = Times (subst x ne) (subst y ne)
subst (Nat x) _ = Nat x
subst (Neg x) ne = Neg (subst x ne) 
subst (Quotient x y) ne = Quotient (subst x ne) (subst y ne)
subst (Div x y) ne = Div (subst x ne) (subst y ne)
subst (Mod x y) ne = Mod (subst x ne) (subst y ne)
subst (NodeEq x y) ne = NodeEq (subst x ne) (subst y ne)
subst (NodeGeq x y) ne = NodeGeq (subst x ne) (subst y ne)
subst (NodeLeq x y) ne = NodeLeq (subst x ne) (subst y ne)
subst (Conj x y) ne = Conj (subst x ne) (subst y ne)
subst PredTrue _ = PredTrue
subst PredFalse _ = PredFalse

free :: [String] -> Node -> [String]

free bound (Var vn) =
  case (elemIndex vn bound) of
    (Just i) -> []
    Nothing -> [vn]

free bound (TypeVar vn) = free bound (Var vn)

free bound (Plus x y) = (free bound x) `union` (free bound y)
free bound (Minus x y) = (free bound x) `union` (free bound y)
free bound (Times x y) = (free bound x) `union` (free bound y)
free bound (Nat x) = []
free bound (Neg x) = free bound x
free bound (Quotient x y) = (free bound x) `union` (free bound y)
free bound (Div x y) = (free bound x) `union` (free bound y)
free bound (Mod x y) = (free bound x) `union` (free bound y)
free bound (NodeEq x y) = (free bound x) `union` (free bound y)
free bound (Conj x y) = (free bound x) `union` (free bound y)

