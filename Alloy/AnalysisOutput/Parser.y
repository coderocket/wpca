{ 
module Alloy.AnalysisOutput.Parser where
import Data.Char
import Alloy.AnalysisOutput.Lexer
}

%name hParse
%tokentype { Token }
%error { parseError }
%monad { Either String } 
%token
  '<:'          { TokRestrict $$ }
  ':'           { TokColon $$ }
  '.'           { TokDot $$ }
  '->'          { TokArrow $$ }
  '='           { TokEq $$ }
  ','           { TokComma $$ }
  '/'           { TokSlash $$ }
  '{'           { TokLCurl $$ }
  '}'           { TokRCurl $$ }
  'this'	{ TokThis $$ }
  'instance'	{ TokInstance $$ }
  'unsat'	{ TokUnsat $$ }
  'check' 	{ TokCheck $$ } 
  word		{ TokWord $$ }
  number	{ TokNumber $$ }

%%

Output : word '.' word Checks { $4 }

Checks : Checks Check { $2:$1 }
  | Check { [$1] }

Check : 'check' Word ':' Result { ($2,$4) }

Result : 'unsat' { Nothing }
  | Instance { Just $1 }

Instance : 'instance' Equations { Instance $2 }

Equations : Equations Equation { $2:$1 }
  | Equation { [$1] }

Equation : Word '=' '{' TuplesOrNothing '}' { Set $1 $4 }
  | Word '/' Word '=' '{' TuplesOrNothing '}' { Set ($1 ++ $3) $6 }
  | 'this' '/' Word '=' '{' TuplesOrNothing '}' { Set $3 $6 }
  | 'this' '/' Word '<:' Word '=' '{' TuplesOrNothing '}' { Relation $3 $5 $8 }

Word : word { snd $1 }

TuplesOrNothing : Tuples { $1 }
  | { [] }

Tuples : Tuples ',' Tuple { $3:$1 }
  | Tuple { [reverse $1] }

Tuple : Tuple '->' Value { $3:$1 }
  | Value { [$1] }

Value : word { snd $1 }
  | number { show (snd $1) }

{

data Instance = Instance [ Equation ]
  deriving (Show)

type Tuple = [ String ]

data Equation = Set String [ Tuple ] | Relation String String [ Tuple ] 
	deriving (Show)

failWithLoc :: AlexPosn -> String -> Either String a
failWithLoc pos err = Left $ err ++ " at " ++ (show line) ++ " line, " ++ (show column) ++ " column\n"
    where
    getLineCol (AlexPn _ line col) = (line,col)
    (line, column) = getLineCol pos

parseError (t:tokens) = failWithLoc (pos t) "parse error\n"

parseError [] = Left "parse error at end of file\n"

alexScanTokens s = runAlex s $ loop 
  where loop = do hd <- alexMonadScan
                  if hd == TokEOF 
                  then return []
                  else do tl <- loop
                          return (hd:tl)

scan s = case (alexScanTokens s) of 
  (Left err) -> error err
  (Right ts) -> ts

parseOutput :: String -> IO [(String, Maybe Instance)]
parseOutput s = case hParse (scan s) of 
  (Left err) -> fail err
  (Right bs) -> return bs

}
