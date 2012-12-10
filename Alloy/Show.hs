module Alloy.Show where

import AST
import Data.Tree
import Data.Ord
import List
import Loc

showA :: AST -> String
showA = foldRose f
  where f (_, Plus) [x,y] = x ++ ".add[" ++ y ++ "]"
        f (_, Minus) [x,y] = x ++ ".sub[" ++ y ++ "]"
        f (_, Times) [x,y] = x ++ ".mul[" ++ y ++"]"
        f (_, Div) [x,y] = x ++ ".div[" ++ y ++"]"
        f (_, Mod) [x,y] = x ++ ".rem[" ++ y ++"]"
        f (_, Quotient) [x,y] = "(" ++ x ++ " / " ++ y ++ ")"
        f (_, Eq) [x,y] = x ++ " = " ++ y
        f (_, NotEq) [x,y] = x ++ " != " ++ y
        f (_, Greater) [x,y] = x ++ " > " ++ y
        f (_, Less) [x,y] = x ++ " < " ++ y
        f (_, Geq) [x,y] = x ++ " >= " ++ y
        f (_, Leq) [x,y] = x ++ " <= " ++ y
        f (_, In) [x,y] = "(" ++ x ++ " in " ++ y ++ ")"
        f (_, Conj) [x,y] = x ++ " and " ++ y
        f (_, Disj) [x,y] = "("++ x ++ " or " ++ y++")"
        f (_, Implies) [x,y] = "(" ++ x ++ " => " ++ y ++")"
        f (_, Range) [x,y] = "range[" ++ x ++ "," ++ y ++ "]"
        f (_, Join) xs = (showJoin xs)
        f (_, Product) xs = (showRel xs)
        f (_, Not) [x] = "!(" ++ x ++ ")"
        f (_, Neg) [x] = x ++ ".negate"
        f (_, Reverse) [x] = "~(" ++ x ++ ")"
        f (_, Int x) [] = show x
        f (_, AST.True) [] = "true"
        f (_, AST.False) [] = "false"
        f (_, Const) [x] = x
        f (_, Type "int") [] = "Int"
        f (_, Type "nat") [] = "Int"
        f (_, Type n) [] = n
	f (_, Output) [x] = x
        f (_, ArrayType _ t) [] = if t == "int" then "seq Int" else ("seq " ++ t)
        f (_, Declaration) [names, typ] = names ++ " : " ++ typ
        f (_, List) names = showNames names
        f (_, Quantifier Sum) [decls, e] = "(sum " ++ decls ++ " | " ++ e ++ ")"
        f (_, Quantifier All) [decls, e] = "(all " ++ decls ++ " | " ++ e ++ ")"
        f (_, Quantifier No) [decls, e] = "(no " ++ decls ++ " | " ++ e ++ ")"
        f (_, Quantifier Some) [decls, e] = "(some " ++ decls ++ " | " ++ e ++ ")"
        f (_, SomeSet) [e] = "(some " ++ e ++ ")"
        f (_, String n) [] = n
        f (_, Pair) [x,y] = "(" ++ x ++ " -> " ++ y  ++ ")"
        f (_, Union) [x,y] = "(" ++ x ++ " + " ++ y  ++ ")"
        f (_, SetDiff) [x,y] = "(" ++ x ++ " - " ++ y  ++ ")"
        f (_, Update) [x,y] = "(" ++ x ++ " ++ " ++ y  ++ ")"
	f (_, Closure) [x] = x
	f other ns = error ("Internal error: Don't know how to show " ++ (show other) ++ " args: " ++ (showNames ns))

showRel = foldr f ""
  where f x [] = x
	f x xs = "(" ++ x ++ ") -> (" ++ xs ++ ")"

showJoin = foldr f ""
  where f x [] = x
        f x xs = "(" ++ x ++ ")." ++ "(" ++ xs ++ ")"

showNames = foldr f ""
  where
    f n [] = n
    f n ns = n ++ "," ++ ns

