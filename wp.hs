module WPC where
import List
import Data.Tree
import AST
import Lexer

-- wp takes a statement and a postcondition in the form of a list of conjuncts and returns the weakest 
-- precondition (in the form of a list of conjuncts) that is necessary for the statement to achieve all 
-- the conjuncts in the postcondition.
--
-- we use the following theorem
--
-- wp s p1 and p2 and ... pn = (wp s p1) and (wp s p2) and ... and (wp s pn)


wp :: AST -> [AST] -> [AST]

wp (Node (_,Assign) [Node (_,List) ns, Node (_,List) es]) post = 
  map (subst (zip (map f ns) es)) post where f (Node (_,String n) []) = n
wp (Node (_,Skip) []) post = post
wp (Node (_,Seq) [x,y]) post = wp x ((wp y) post)
wp (Node (_,Cond) gs) post = (altguards gs) : (wpcond post gs)

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
{-
wp (Loop inv gs) post = inv : (foldr (++) [] (map f gs)) ++ (map (h gs) post) 
  where f (g,s) = map (implies (inv `conj` g)) (wp s [inv])
        h gs p = (inv `conj` (foldr conj PredTrue (map (Not . fst) gs))) `implies` p
-}

wp (Node (_,Loop) [inv, (Node (_,List) gs)]) post = 
  [annotate "loop_invariant" inv] ++ (foldr (++) [] (map f gs)) ++ (map ((annotate "loop_final").(h gs)) post)
  where f (Node (_,List) [g,s]) = map ((annotate "guard").(implies (g `conj` inv))) (wp s [inv])
        h gs p = (inv `conj` (foldr conj true (map (AST.not . head . subForest) gs))) `implies` p

altguards = (annotate "allguards") . (foldr f false)
   where f (Node (_,List) [g,s]) ps = g `disj` ps

wpcond post = foldr f [] 
   where f (Node (_,List) [g,s]) ps = (map ((annotate "guard").(implies g)) (wp s post)) ++ ps

subst :: [(String,AST)] -> AST -> AST

subst env (Node (p,String n) []) = 
  case (lookup n env) of 
	(Just e) -> e
	Nothing -> Node (p, String n) []

subst env (Node n ns) = Node n (map (subst env) ns) 

free :: [String] -> AST -> [String]

free bound (Node (_,String n) []) =
  case (elemIndex n bound) of
    (Just i) -> []
    Nothing -> [n]

free bound (Node _ ns) = foldr union [] (map (free bound) ns)

