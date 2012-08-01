
module Main where
import System
import Parser
import Config.Parser
import AST
import Alloy.Analyzer
import CLang.Generator

main = 
  do putStr versionMsg 
     args <- getArgs
     pname <- getProgName
     if length args /= 1
     then usage pname
     else do s <- readFile "wpca.cfg"
             cfg <- parseConfig s
             s <- readFile (args!!0)
             ast <- parse s
             work cfg ast
             CLang.Generator.generate cfg ast

versionMsg = "This is wpca, Version 1.0\n"

usage pname = putStrLn ("usage: " ++ pname ++ " spec")


