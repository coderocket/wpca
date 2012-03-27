
module Main where
import System
import Grammar
import CFGGrammar
import AST
import Alloy
import CLang

main = 
  do putStr versionMsg 
     args <- getArgs
     pname <- getProgName
     if length args /= 1
     then usage pname
     else do s <- readFile "wpca.cfg"
             env <- parseConfig s
             s <- readFile (args!!0)
             ast <- parse s
             showAlloy env ast
             showCCode env ast

versionMsg = "This is wpca, Version 1.0\n"

usage pname = putStrLn ("usage: " ++ pname ++ " spec")


