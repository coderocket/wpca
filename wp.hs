module WPC where
import List
import AST

-- wp takes a statement and a postcondition in the form of a list of conjuncts and returns the weakest 
-- precondition (in the form of a list of conjuncts) that is necessary for the statement to achieve all 
-- the conjuncts in the postcondition.
--
-- we use the following theorem
--
-- wp s p1 and p2 and ... pn = (wp s p1) and (wp s p2) and ... and (wp s pn)

wp :: Node -> [Node] -> [Node]

wp (Assign (ns,es)) post = map (subst (zip ns es)) post
wp (Cond gs) post = (altguards gs) : (wpcond post gs)
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

wp (Loop inv gs) post = inv : (foldr (++) [] (map f gs)) ++ (map (h gs) post) 
  where f (g,s) = map (implies (inv `conj` g)) (wp s [inv])
        h gs p = (inv `conj` (foldr conj PredTrue (map (Not . fst) gs))) `implies` p


altguards [(g,s)] = g
altguards ((g,s):gs) = BinOp Disj g (altguards gs)

wpcond post [] = []
wpcond post ((g,s):gs) = (map (BinOp Implies g) (wp s post)) ++ (wpcond post gs)

subst :: [(String,Node)] -> Node -> Node

subst env (Var vn) = 
  case (lookup vn env) of 
	(Just e) -> e
	Nothing -> (Var vn)
subst env (BinOp k x y) = BinOp k (subst env x) (subst env y)
subst env (Neg x) = Neg (subst env x) 
subst env (Not n) = Not (subst env n)
subst _ x = x

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
free bound (Not x) = free bound x

