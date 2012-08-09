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
wpx (Node (pos,Assert) [p]) post = (p,[pos],"satisfy the assertion"):post
wpx (Node (_,Seq) [x,y]) post = wpx x ((wpx y) post)

wpx (Node (pos,Cond) gs) post = ifdomain : guards 
  where ifdomain = (foldr disj false (map guard gs), [pos], "satisfy any of the guards")
        guards = [ (g `implies` p, (npos s):path, goal) |  (g,s) <- map tidy gs, (p, path, goal) <- wpx s post ]

{- 

The predicates in maintainInv are not a part of the precondition but
independent goals that should be checked on their own. To illustrate
this problem consider the following code:

; n := 10
  { true }
keeping 
  n >= 0
do n > 0 -> n := n - 1 od
  { n = 0 }

In this example we must ensure that the assignment n := 10 establishes
the invariant n >= 0 but then we have to separately check

{ n >= 0 and n > 0 }
n := n - 1
{ n >= 0 }

That is, that

n >= 0 and n > 0 => wp(n := n-1, n>=0)

But currently we have a major flaw because instead we check that

  wp (n := 10, (n >= 0 and n > 0) => wp(n := n-1, n>=0))

One approach:

wpx should return two lists: one is a list of preconditions and the
other is a list of top-level obligations.  The top-level obligations
are just collected in a list. Only the preconditions are used in the
wp calculations.

Another approach:

It is as if the loop construct is a separate command that has the
following contract

precondition : loop invariant and invariant maintenance assertions
body 
postcondition : loop invariant and negation of all guards 

Therefore we can use the general form of the wp of a specification
statement:

wp (pre,post) p = pre and (all v:state | post => p)

The quantifier protects from substituting in post. However this form
requires that post will contain all the state variables that do not
change.  We can add them by examining the loop and extracting all the
state variables that are assigned to in the loop. Then we can use the
wp for a specification statement with a frame: 

wp (pre,frame,post) p = pre and (all v:frame | post => p)

Actually we cannot quantify over the state variables because Alloy can
quantify only over atoms and this rules out quantifying over array state
variables. Instead we can quantify over the state itself. This is very
easy to achieve because it means that to universally quantify over the
state variables of a predicate we simply prevent substitution on that
predicate using a special 'closure' node.

-}

wpx (Node (pos,Loop) [inv, (Node (_,List) gs)]) post = establishInv : maintainInv ++ achieveGoals
  where establishInv = (inv, [], "establish the loop invariant at " ++ (show pos))
        maintainInv = [ (close ((g `conj` inv) `implies` p), (npos g):path, goal) | (g,s) <- map tidy gs, (p,path,goal) <- wpx s [(inv, [], "maintain the loop invariant at " ++ (show pos))] ]
        achieveGoals = [(close (inv `conj` (foldr conj true [ AST.not g | (g,_) <- map tidy gs ]) `implies` p), pos:path, goal) | (p,path,goal) <- post ]

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
  substQuantifier p q bound env decls body
subst bound env (Node (_, Closure) [n]) = n
subst bound env (Node n ns) = Node n (map (subst bound env) ns) 

substQuantifier pos kind bound env decls body =
  Node (pos, Quantifier kind) [newDecls, newBody]
  where newBody = subst ((declNames decls)++bound) env body
        newDecls = Node (pos, List) [ Node (p, Declaration) [ns, subst bound env t] | (Node (p, Declaration) [ns, t]) <- (subForest decls) ]

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

