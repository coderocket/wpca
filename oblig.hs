

module Oblig where

import WPX
import Subst
import Data.List

type Oblig = (Pred, Stmt, Pred)

oblig :: Stmt -> [Oblig] -> [Oblig]

oblig (Comp s1 s2) obs = oblig s1 (oblig s2 obs)
oblig (Cond guards) obs = foldr obligGuard obs guards
oblig (Loop inv frame guards) obs = 
	foldr obligGuard ([(LogicOp Conj [inv, g], s, inv) | (Guard g s) <- guards] ++ obs) guards
oblig _ obs = obs 

obligGuard (Guard g s) rest = oblig s rest

