

module WPX where

import Subst
import Data.List


data Stmt = Skip | Assign Def | Comp Stmt Stmt | Cond [Guard] 

data Guard = Guard Pred Stmt

type Loc = Pred

type Path = [Loc]

class WPX a where
  wpx :: a -> [Pred] -> [Pred]
  ptree :: a -> [Path] -> [Path]

instance WPX Stmt where
  wpx Skip pre = pre
  wpx (Comp s1 s2) pre = wpx s1 (wpx s2 pre)
  wpx (Assign def) pre = map (subst def) pre
  wpx (Cond guards) pre = foldr (++) [] [ wpx g pre | g <- guards ]

  ptree (Cond guards) path = foldr (++) [] [ ptree g path | g <- guards ]
  ptree (Comp s1 s2) path = ptree s1 (ptree s2 path)
  ptree _ path = path

instance WPX Guard where
  wpx (Guard g s) pre = [ LogicOp Impl [g, q] | q <- wpx s pre ]
  ptree (Guard g s) path = [ g : p | p <- ptree s path ]
