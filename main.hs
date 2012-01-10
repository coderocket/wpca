
module Main where
import System
import Grammar
import ParserMonad
import CFGGrammar
import AST
import Alloy
import CLang

work (Ok env) (Ok ast) = 
  case (lookup "global.analysisfile" env) of
    (Just (n:ns)) -> 
      do putStrLn ("writing analysis file to " ++ n)
         writeFile n (showAlloy ast)
         writeCFile env ast
    Nothing -> putStrLn "error: configuration is missing analysis file name."

work (Failed err) _ = putStrLn ("failed parsing config file:" ++ err)
work _ (Failed err) = putStrLn ("failed parsing program: " ++ err)

writeCFile env ast =
  case (lookup "c.sourcefile" env) of
    (Just (n:ns)) ->
     do putStrLn ("writing c source file to " ++ n)
        writeFile n (showCCode "main" ast)
    Nothing -> putStrLn ""

main = do putStr versionMsg 
          args <- getArgs
          pname <- getProgName
          if length args /= 1
             then usage pname
             else do s <- readFile "wpca.cfg"
                     c <- return (parseConfig s)
                     s <- readFile (args!!0)
                     p <- return (parse s)
                     work c p

versionMsg = "This is wpca, Version 1.0\n"

usage pname = putStrLn ("usage: " ++ pname ++ " spec")


