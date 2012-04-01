module WPC where
import List
import Data.Tree
import AST
import Lexer

type Oblig = (AST, [AlexPosn], String)

name (Node (_, String n) []) = n
guard (Node (_,List) [g,s]) = g 
tidy (Node (_,List) [g,s]) = (g,s)
npos = fst . rootLabel 

wpx :: AST -> [Oblig] -> [Oblig]
wpx (Node (_,Assign) [Node (_,List) ns, Node (_,List) es]) post = 
  [ (subst (zip (map name ns) es) p, path, goal) | (p,path,goal) <- post ]

wpx (Node (_,Skip) []) post = post
wpx (Node (_,Seq) [x,y]) post = wpx x ((wpx y) post)

wpx (Node (pos,Cond) gs) post = ifdomain : guards 
  where ifdomain = (foldr disj false (map guard gs), [pos], "satisfy any of the guards")
        guards = [ (g `implies` p, (npos s):path, goal) |  (g,s) <- map tidy gs, (p, path, goal) <- wpx s post ]

wpx (Node (pos,Loop) [inv, (Node (_,List) gs)]) post = establishInv : maintainInv ++ achieveGoals
  where establishInv = (inv, [pos], "establish the loop invariant")
        maintainInv = [ ((g `conj` inv) `implies` p, (npos g):path, goal) | (g,s) <- map tidy gs, (p,path,goal) <- wpx s [(inv, [pos], "maintain the loop invariant")] ]
        achieveGoals = [(inv `conj` (foldr conj true [ AST.not g | (g,_) <- map tidy gs ]) `implies` p, pos:path, goal) | (p,path,goal) <- post ]

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

