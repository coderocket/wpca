module Alloy.WPC where
import Data.List
import Data.Tree
import AST
import Loc
import Alloy.Show

type Oblig = (AST, [(Loc,String)], String)

name (Node (_, String n) []) = n
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
collect bs = simple ++ [ (n, (string n) `update` expr) | (n,expr) <- reduce arrays ]
  where (simple,arrays) = separate bs

separate :: [(AST,AST)] -> (Env, Env)
separate = foldr f ([],[])
  where f (Node (pos, String n) [], expr) (simple,arrays) = ((n,expr):simple,arrays)
        f (Node (pos, Join) [index, Node (_, String n) []], expr) (simple,arrays) = (simple, (n, pair index expr):arrays)
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

wpx :: Env -> AST -> [Oblig] -> [Oblig]
wpx procs (Node (_,Assign) [Node (_,List) ns, Node (_,List) es]) post = 
  [ (subst [] (collect (zip ns es)) expr, path, goal) | (expr,path,goal) <- post ]

{-

wp (x is new T) P = some (T - extent) and all x : T - extent | P[extent := extent + x])

-}

-- because we quantify over state variables we must first replace every occurence of the state
-- variable with a plain (string) variable.

wpx procs (Node (_,Alloc name) [typ]) post = [ (f p, path, goal) | (p, path, goal) <- post ]
  where f p = (someSet fresh) `conj` (AST.all name fresh (subst [] [(name, string name), ("extent", (string "extent") `AST.union` (string name))] p))
        fresh = typ `setDiff` (string "extent")


wpx procs (Node (_,Skip) []) post = post
wpx procs (Node (pos,Assert) [p]) post = (p,[(pos,showA p)],"satisfy the assertion"):post
wpx procs (Node (_,Seq) [x,y]) post = wpx procs x ((wpx procs y) post)
  
wpx procs (Node (pos,Cond) gs) post = ifdomain : guards 
  where ifdomain = (foldr disj false (map guard gs), [(pos,"if ... fi")], "satisfy any of the guards")
        guards = [ (g `implies` p, (npos s, showA g):path, goal) |  (g,s) <- map tidy gs, (p, path, goal) <- wpx procs s post ]

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

wpx procs (Node (pos,Loop) [inv, (Node (_,List) gs)]) post = establishInv : maintainInv ++ achieveGoals
  where establishInv = (inv, [], "establish the loop invariant\n\n" ++ (show (fst pos)) ++ ": " ++ (showA inv))
        maintainInv = [ (close ((g `conj` inv) `implies` p), (npos g, showA g):path, goal) | (g,s) <- map tidy gs, (p,path,goal) <- wpx procs s [(inv, [], "maintain the loop invariant\n\n" ++ (show (fst pos)) ++ " : " ++ (showA inv))] ]
        achieveGoals = [(close (postLoop `implies` p), (pos,(showA postLoop) ++ "\n\t {the invariant and the negation of the guards}\n"):path, goal) | (p,path,goal) <- post ]
        postLoop = inv `conj` (foldr conj true [ AST.not g | (g,_) <- map tidy gs ]) 

{-

Given a procedure f[params] modifies xs {pre} {post})

The semantics of the call f[args] is the following:

(pre[params:=args] | out[params:=args],xs | post[params:=args])

where out is the set of out parameters and provided that in the
substitution params:=args the output parameters are replaced by variables.

In addition, the wp of a specification statement is:

wp (pre | out | post) P = some constants | pre and (all out | post => P)

where constants are the constant variables that are defined implicitly
in the precondition (an alternative is to avoid using the existential quantifier
by immediately substituting the constants by their variables).

We create this program fragment as follows:

1. To generate the first assignment we use the function assignToParams
that takes the parameters of the procedure and the arguments of the call
and returns the appropriate assignment statement.

2. To generate the specification statement and the final assignment we
use the functions filterOutParams and filterOutVars to extract the names
of the output parameters from params and to extract the output variables
from args. 

-}
 
wpx procs (Node (pos,Call name) args) obligs =
	case (lookup name procs) of
		(Just proc) -> wpxCall pos procs proc args obligs
		Nothing -> error ("No such procedure: " ++ name)

wpx procs (Node ((line,_), SpecStmt) [pre, constants, frame, post]) obligs = 
	[ (onePointRule constants pre, [], "satisfy the precondition\n" ++ (showA pre) ++ "\nof the procedure call at line " ++ (show line) ++"\n") ]
	++ [ (subst [] (swapNames (freshConstants p)) (onePointRule constants (quantifyState frame (post `implies` (subst [] (freshConstants p) p)))), path, goal) | (p,path,goal) <- obligs ]
	  where freshConstants p = freshNames (constantNames constants) p

freshNames :: [String] -> AST -> [(String, AST)]
freshNames used expr = map f (used `intersect` (free [] expr)) 
  where f n = (n, string (genFresh n 0 used))

swapNames :: [(String, AST)] -> [(String, AST)]
swapNames ns = [ (n2, string n1) | (n1, Node (_,String n2) []) <- ns ]

constantNames :: AST -> [String]
constantNames = map f . subForest
  where f (Node (_,Declaration) [(Node (_,List) [Node (_,String x) [],e]), t]) = x
  
onePointRule :: AST -> AST -> AST 
onePointRule (Node (_,List) constants) pred = subst [] (map f constants) pred
  where f (Node (_,Declaration) [(Node (_,List) [Node (_,String x) [],e]), t]) = (x,e)

quantifyState decls pred = 
  case decls of
	(Node (_,List) []) -> error "Invalid procedure definition (empty frame)."
	_ -> if fresh `elem` (free [] pred) 
	     then error (show pred) 
	     else AST.all fresh (string "STATE") (subst [] joins pred)
  where joins = [ (name, Node (startLoc, Join) [string fresh, string ("STATE_" ++ name)]) | name <- names ]
        names = map fst (declsToList (subForest decls))
        fresh = genFresh "STATE_DUMMY" 0 (bound pred)

-- note: we assume that the generated name could clash only with bound variables. a more robust 
-- way is to ensure that the name does not clash with any variable.

genFresh :: String -> Int -> [String] -> String
genFresh name suffix bs = 
  if (name ++ "_" ++ (show suffix)) `elem` bs
  then genFresh name (suffix+1) bs
  else name ++ "_" ++ (show suffix)

quantifyAll decls pred = case decls of 
				(Node (_,List) []) -> error "Invalid procedure definition (empty frame)."
				_ -> Node (startLoc, Quantifier All) [decls, pred]

wpxCall pos procs (Node (_,Proc _) [params,locals,constants,pre,body,post,modifies]) args obligs = 
  wpx procs (Node (pos, SpecStmt) [pre', constants', frame, post']) obligs 
  where  pre' = subst [] env pre
         post' = subst [] env post
         constants' = Node (startLoc, List) (map f (subForest constants))
         f (Node (_,Declaration) [(Node (_,List) [Node (_,String x) [],e]), t]) = 
             Node (startLoc, Declaration) [Node (startLoc,List) [Node (startLoc, String x) [], subst [] env e], t]
         frame = Node (startLoc, List) ([ declaration name t | ((_, Node (_,Output) [t]), Node (_,String name) []) <- zip paramList args ] ++ (subForest modifies))
         env = zip (map fst paramList) args
         paramList = declsToList (subForest params)
         
getNames :: AST -> AST
getNames decls = Node (startLoc,List) [ string name | (name,_) <- (declsToList (subForest decls)) ]

filterOutParams :: AST -> AST
filterOutParams = Node (startLoc, List) . foldr f [] . subForest
  where f d rest = case d of 
			(Node (_,Declaration) [_, Node (_,Output) _]) -> d:rest
			_ -> rest

filterOutVars :: [(AST,AST)] -> AST
filterOutVars = Node (startLoc, List) . foldr f []
  where f (p,e) rest = 
	 case e of 
	 (Node (_, String _) []) -> 
		 case p of 
		 (Node (_,Declaration) [_, Node (_,Output) _]) -> e:rest
		 _ -> rest
	 _ -> rest

{- subst bound new expr  

substitutes all the free occurrences of state variables in 
expr that have a binding in new by their binding in new.

-}

subst :: [String] -> Env -> AST -> AST

subst bound env (Node (p,String n) []) = 
  case (elemIndex n bound) of  
    Nothing -> case (lookup n env) of 
                (Just e) -> let captured = (nub bound) `intersect` (free [] e)
                               in case captured of 
                                   [] -> e
                                   _ -> error ("Can't substitute " ++ (show e) ++ "for " ++ n ++ " because " ++ (show bound) ++ " are bound in the expression but free in " ++ (show e))
                Nothing -> Node (p, String n) []
    (Just _) -> Node (p, String n) []

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
free bound (Node (_, Quantifier _) [decls,body]) = freeQuantifier bound decls body
free bound (Node _ ns) = foldr Data.List.union [] (map (free bound) ns)

freeVar bound n = 
  case (elemIndex n bound) of
    (Just i) -> []
    Nothing -> [n]

freeQuantifier bound decls body =
  free ((declNames decls)++bound) body

bound :: AST -> [String]

bound (Node (_, Quantifier _) [decls,body]) = boundQuantifier decls body
bound (Node _ ns) = foldr Data.List.union [] (map bound ns)

boundQuantifier decls body =
  (declNames decls) `Data.List.union` (bound body)

declNames :: AST -> [String]
declNames decls = map fst (declsToList (subForest decls))

