
module Main where
import System
import Grammar
import CFGGrammar
import AST
import Alloy
import CLang

work (Right env) (Right ast) = 
  do p <- showAlloy env ast
     q <- showCCode env ast
     return (p >> q)

work (Left err) _ = Just $ putStrLn ("invalid configuration file:" ++ err)
work _ (Left err) = Just $ putStrLn ("invalid program: " ++ err)

main = 
  do putStr versionMsg 
     args <- getArgs
     pname <- getProgName
     if length args /= 1
     then usage pname
     else do s <- readFile "wpca.cfg"
             c <- parseConfig s
             s <- readFile (args!!0)
             p <- parse s
             case (work c p) of 
               (Just m) -> m
               Nothing -> putStrLn "error: invalid configuration file: missing declaration."          

versionMsg = "This is wpca, Version 1.0\n"

usage pname = putStrLn ("usage: " ++ pname ++ " spec")


