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

wpx :: AST -> [Oblig] -> [Oblig]
wpx (Node (_,Assign) [Node (_,List) ns, Node (_,List) es]) post = 
  [ (subst [] (zip (map name ns) es) p, path, goal) | (p,path,goal) <- post ]

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

subst bound env (Node (p, Sum) [decls, body]) = 
  substQuantifier p Sum bound env decls body
subst bound env (Node (p, All) [decls, body]) = 
  substQuantifier p All bound env decls body
subst bound env (Node (p, No) [decls, body]) = 
  substQuantifier p No bound env decls body
subst bound env (Node n ns) = Node n (map (subst bound env) ns) 

substQuantifier pos kind bound env decls body =
  Node (pos, kind) [decls, newBody]
  where newBody = subst ((declNames decls)++bound) env body

free :: [String] -> AST -> [String]

free bound (Node (_,String n) []) = freeVar bound n
free bound (Node (_,StateVar n) []) = freeVar bound n
free bound (Node (_,ConstVar n) []) = freeVar bound n
free bound (Node (_, Sum) [decls,body]) = freeQuantifier bound decls body
free bound (Node (_, All) [decls,body]) = freeQuantifier bound decls body
free bound (Node (_, No) [decls,body]) = freeQuantifier bound decls body
free bound (Node _ ns) = foldr union [] (map (free bound) ns)

freeVar bound n = 
  case (elemIndex n bound) of
    (Just i) -> []
    Nothing -> [n]

freeQuantifier bound decls body =
  free ((declNames decls)++bound) body

declNames :: AST -> [String]
declNames decls = map fst (declsToList (subForest decls))

