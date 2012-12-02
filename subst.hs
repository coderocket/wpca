
module Subst where

import Fresh
import Data.List

data Expr = Lit Int | Var Name
  deriving (Show)

type Def = [(Name, Expr)]

data Pred =  RelOp RelOpTag [Expr] | LogicOp LogicOpTag [Pred] | Quantifier QuantifierTag Def Pred
  deriving (Show)

data RelOpTag = In | Equal
  deriving (Show)

data QuantifierTag = All | Some
  deriving (Show)

data LogicOpTag = Conj | Disj | Impl | Neg
  deriving (Show)

class Subst a where
  subst :: Def -> a -> a

instance Subst Pred where
  subst bs (RelOp t es) = RelOp t (subst bs es)
  subst bs (LogicOp t ps) = LogicOp t (subst bs ps)
  subst bs q@(Quantifier t def p) = 
   case captured of 
    [] -> Quantifier t [(n, subst bs e) | (n,e) <- def] (subst (filter (\(n,e) -> n `notElem` (map fst def)) bs) p)
    _ -> subst bs (rename captured bs q) 
   where captured = (map fst def) `intersect` (free (map snd bs))

rename :: [Name] -> Def -> Pred -> Pred

rename captured bs (Quantifier t def p) = 
    Quantifier t (update freshNames def) (subst [ (n,Var n') | (n,n') <- freshNames] p)
   where 
     freshNames = zip captured (reverse (fresh captured ((free p) `union` (free (map snd bs)))))

update :: [(Name,Name)] -> Def -> Def

update new def = [ (f n, e) | (n,e) <- def ]
  where f n = case lookup n new of 
                   Just n' -> n'
                   Nothing -> n

instance Subst Expr where 
  subst bs (Var n) = case lookup n bs of 
                      Just e -> e
                      Nothing -> Var n
  subst bs e = e

instance Subst a => Subst [a] where
  subst bs = map (subst bs)

class Free a where
  free :: a -> [Name]

instance Free Pred where
  free (RelOp _ es) = free es
  free (LogicOp _ ps) = free ps
  free (Quantifier _ def p) = (free p \\ (map fst def)) `union` (free (map snd def))

instance Free Expr where
  free (Var n) = [n]
  free _ = []

instance Free a => Free [a] where
  free xs = foldr union [] (map free xs)

