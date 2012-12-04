
module WPX where

import Subst
import Data.List

data Stmt = Skip | Assign Def | Comp Stmt Stmt | Cond [Guard] | Spec Pred Def Pred | Loop Pred Def [Guard]

data Guard = Guard { guard :: Pred , stmt :: Stmt }

type Loc = Pred

type Path = [Loc]

class WPX a where
  wpx :: a -> [Pred] -> [Pred]
  ptree :: a -> [Path] -> [Path]

instance WPX Stmt where
  wpx Skip post = post
  wpx (Comp s1 s2) post = wpx s1 (wpx s2 post)
  wpx (Assign def) post = map (subst def) post
  wpx (Cond guards) post = foldr (++) [] [ wpx g post | g <- guards ]
  wpx (Spec pre frame post) post' = 
    [LogicOp Conj [pre, Quantifier All frame (post `implies` p)] | p <- post'] 
    where implies p q = LogicOp Impl [p,q]
  wpx (Loop inv frame guards) post = wpx (Spec inv frame (LogicOp Conj [inv, negate guards])) post 
    where negate guards = LogicOp Not [LogicOp Disj (map guard guards)]

  ptree (Cond guards) path = foldr (++) [] [ ptree g path | g <- guards ]
  ptree (Comp s1 s2) path = ptree s1 (ptree s2 path)
  ptree _ path = path

instance WPX Guard where
  wpx (Guard g s) post = [ LogicOp Impl [g, q] | q <- wpx s post ]
  ptree (Guard g s) path = [ g : p | p <- ptree s path ]

