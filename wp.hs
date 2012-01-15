module WPC where
import List
import AST

wp :: Node -> Node -> Node

wp (Assign (ns,es)) post = subst post (zip ns es)
wp (Cond gs) post = BinOp Conj (altguards gs) (wpcond post gs)
wp (Seq x y) post = wp x ((wp y) post)
wp Skip post = post

{-

because we can't calculate the weakest precondition of a loop 
we use the following method:

given the loop 

keeping inv do g1 -> s1 [] g2 -> s2 [] ... gn -> sn od

(the invariant is inv) we check the following proof obligations:

all i :1..n | (inv and gi) => wp(si,inv)

this means that when we exit the loop (currently we do not check
for termination) the following must hold:

inv and !g1 and !g2 and ... and !gn

now, to find the 'wp' of the loop on an arbitrary postcondition 
we use the following definition of the wp of a specification statement:

wp (pre,post) p = pre and (post => p)

so we calculate

wp (keeping inv do g1 -> s1 [] g2 -> s2 [] ... gn -> sn od, p) =
  inv and (inv and g1 => wp(s1,inv) and ... (inv and gn => wp(sn,inv) and (inv and !g1 and !g2 and ... and !gn => p)

-}

wp (Loop inv gs) post = BinOp Conj inv (foldr (BinOp Conj) PredTrue (map f gs)) where f (g,s) = BinOp Implies (BinOp Conj inv g) (wp s inv)

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

