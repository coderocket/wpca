module WPC where
import List
import AST

wp :: Node -> Node -> Node

wp (Assign (ns,es)) post = subst post (zip ns es)
wp (Cond gs) post = BinOp Conj (altguards gs) (wpcond post gs)
-- wp (Loop gs) post =
wp (Seq x y) post = wp x ((wp y) post)
wp Skip post = post

altguards [(g,s)] = g
altguards ((g,s):gs) = BinOp Disj g (altguards gs)

wpcond post [(g,s)] = BinOp Implies g (wp s post) 
wpcond post ((g,s):gs) = BinOp Conj (BinOp Implies g (wp s post)) (wpcond post gs)

subst :: Node -> [(String,Node)] -> Node

subst (Var vn) ne = 
  case (lookup vn ne) of 
	(Just e) -> e
	Nothing -> (Var vn)
subst (TypeVar vn) ne = TypeVar vn
subst (BinOp k x y) ne = BinOp k (subst x ne) (subst y ne)
subst (Nat x) _ = Nat x
subst (Neg x) ne = Neg (subst x ne) 
subst PredTrue _ = PredTrue
subst PredFalse _ = PredFalse

free :: [String] -> Node -> [String]

free bound (Var vn) =
  case (elemIndex vn bound) of
    (Just i) -> []
    Nothing -> [vn]

free bound (TypeVar vn) = free bound (Var vn)

free bound (BinOp _ x y) = (free bound x) `union` (free bound y)
free bound (Neg x) = free bound x
free bound (Nat x) = []
free bound PredTrue = []
free bound PredFalse = []

