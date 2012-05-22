module Alloy.WPC where
import List
import Data.Tree
import AST
import Loc

type Oblig = (AST, [Loc], String)

name (Node (_, String n) []) = n
name (Node (_, StateVar n) []) = n
guard (Node (_,List) [g,s]) = g 
tidy (Node (_,List) [g,s]) = (g,s)
npos = fst . rootLabel 

{- The semantics of simple assignment is simple:

wp (x := e) P = P[x/e]

where P[x/e] is the substitution of every free occurence of x in P by
the expression e provided that e does not contain free variables that
are bound in P.

We extend assignment to multiple variables and to array cells. For
example:

A[i],A[j] := A[j],A[i]

Thus the left hand side of an assignment can now be not only a simple
variable but in general an L-value. An L-value is either a variable
or an array expression.

In order to support such assignment statements we convert them into 
simple assignments according to the following rule:

- We collect all the assignments to the same array into a single
update operation. For the example above we get:

A := A (++) { i->A[j], j->A[i] }

This is the purpose of the function collect. It takes a list of pairs
that consist of an L-value and an expression to assign to this L-value
and returns an equivalent list in which all the assignments to the same
array have been collected according to the rule above.

We implement collect in two steps. First we separate the list of
multiple assignemnts into two lists: one for simple variables and
one for assignment to array cells. Then we collect the second list
according to the rule we have described above.

Assume that all the pairs in the input have already been collected in
this manner except the first pair. To complete the collection we face
the following alternatives:

1. The L-value in the first pair is a simple variable. If there is already
a pair with this variable in the list we flag an error as it is illegal
to assign twice into the same simple variable.

2. The L-value in the first pair is an array expression and the name
of the array does not appear in the output list. In this case we simply
add the pair to the list.

3. The L-value in the fist pair is an array expression and the name
of the array appears in the output list. In this case we add to the pair
in the output list the pair that consists of the array index and the
assigned expression.

-}

collect :: [(AST,AST)] -> Env
collect bs = simple ++ [ (n, (stateVar n) `update` expr) | (n,expr) <- reduce arrays ]
  where (simple,arrays) = separate bs

separate :: [(AST,AST)] -> (Env, Env)
separate = foldr f ([],[])
  where f (Node (pos, StateVar n) [], expr) (simple,arrays) = ((n,expr):simple,arrays)
        f (Node (pos, Join) [index, Node (_, StateVar n) []], expr) (simple,arrays) = (simple, (n, pair index expr):arrays)
        f n _ = error ("Invalid L-value: " ++ (show n))

reduce :: Env -> Env
reduce = foldr f []
  where f (n, expr) xs = 
         case (lookup n xs) of 
          Nothing -> (n, expr):xs
          (Just pairs) -> replace xs n (expr `AST.union` pairs) 

replace :: [(String, a)] -> String -> a -> [(String, a)]
replace [] _ _ = []
replace ((n,x):assoc) m y = 
  if n == m then
    (n,y):assoc
  else 
    (n,x):(replace assoc m y)

wpx :: AST -> [Oblig] -> [Oblig]
wpx (Node (_,Assign) [Node (_,List) ns, Node (_,List) es]) post = 
  [ (subst [] (collect (zip ns es)) expr, path, goal) | (expr,path,goal) <- post ]

wpx (Node (_,Skip) []) post = post
wpx (Node (_,Seq) [x,y]) post = wpx x ((wpx y) post)

wpx (Node (pos,Cond) gs) post = ifdomain : guards 
  where ifdomain = (foldr disj false (map guard gs), [pos], "satisfy any of the guards")
        guards = [ (g `implies` p, (npos s):path, goal) |  (g,s) <- map tidy gs, (p, path, goal) <- wpx s post ]

wpx (Node (pos,Loop) [inv, (Node (_,List) gs)]) post = establishInv : maintainInv ++ achieveGoals
  where establishInv = (inv, [], "establish the loop invariant at " ++ (show pos))
        maintainInv = [ ((g `conj` inv) `implies` p, (npos g):path, goal) | (g,s) <- map tidy gs, (p,path,goal) <- wpx s [(inv, [], "maintain the loop invariant at " ++ (show pos))] ]
        achieveGoals = [(inv `conj` (foldr conj true [ AST.not g | (g,_) <- map tidy gs ]) `implies` p, pos:path, goal) | (p,path,goal) <- post ]

{- The call

subst bound new expr  

substitutes all the free occurrences of state variables in 
expr that have a binding in new by their binding in new.

substitution affects only state variables because we can only 
assign to state variables. 

-}
 
subst :: [String] -> Env -> AST -> AST

subst bound env (Node (p,StateVar n) []) = 
  case (elemIndex n bound) of  
    Nothing -> case (lookup n env) of 
                (Just e) -> let captured = (nub bound) `intersect` (free [] e)
                               in case captured of 
                                   [] -> e
                                   _ -> error ("captured: " ++ (show captured))
                Nothing -> Node (p, StateVar n) []
    (Just _) -> Node (p, StateVar n) []

subst bound env (Node (p, Quantifier q) [decls, body]) = 
  substQuantifier p q bound env (subst bound env decls) body
subst bound env (Node n ns) = Node n (map (subst bound env) ns) 

substQuantifier pos kind bound env decls body =
  Node (pos, Quantifier kind) [newDecls, newBody]
  where newBody = subst ((declNames decls)++bound) env body
        newDecls = Node (pos, Locals) [ Node (p, Declaration) [ns, subst bound env t] | (Node (p, Declaration) [ns, t]) <- (subForest decls) ]

free :: [String] -> AST -> [String]

free bound (Node (_,String n) []) = freeVar bound n
free bound (Node (_,StateVar n) []) = freeVar bound n
free bound (Node (_,ConstVar n) []) = freeVar bound n
free bound (Node (_, Quantifier _) [decls,body]) = freeQuantifier bound decls body
free bound (Node _ ns) = foldr List.union [] (map (free bound) ns)

freeVar bound n = 
  case (elemIndex n bound) of
    (Just i) -> []
    Nothing -> [n]

freeQuantifier bound decls body =
  free ((declNames decls)++bound) body

declNames :: AST -> [String]
declNames decls = map fst (declsToList (subForest decls))

