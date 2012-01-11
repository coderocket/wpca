
module Main where
import System
import Grammar
import CFGGrammar
import AST
import Alloy
import CLang

work (Right env) (Right ast) = 
  do ns <- lookup "global.analysisfile" env
     cs <- lookup "c.sourcefile" env
     return $ do putStrLn ("writing analysis file to " ++ (head ns))
                 writeFile (head ns) (showAlloy ast)
                 putStrLn ("writing c source file to " ++ (head cs))
                 writeFile (head cs) (showCCode "main" ast)

 
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


