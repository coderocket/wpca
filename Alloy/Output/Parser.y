{ 
module Alloy.Output.Parser where
import Data.Char
import Alloy.Output.Lexer
}

%name hParse
%tokentype { Token }
%error { parseError }
%monad { Either String } 
%token
  '<:'          { TokRestrict $$ }
  ':'           { TokColon $$ }
  '->'          { TokArrow $$ }
  '='           { TokEq $$ }
  ','           { TokComma $$ }
  '/'           { TokSlash $$ }
  '{'           { TokLCurl $$ }
  '}'           { TokRCurl $$ }
  'skolem'	{ TokSkolem $$ }
  'instance'	{ TokInstance $$ }
  'unsat'	{ TokUnsat $$ }
  'check' 	{ TokCheck $$ } 
  word		{ TokWord $$ }
  number	{ TokNumber $$ }

%%

Output : Checks { reverse $1 }

Checks : Checks Check { $2:$1 }
  | Check { [$1] }

Check : 'check' Word Scope ':' Result { ($2,$5) }

Scope :  { [] }
  | Scope word { [] }
  | Scope number { [] }

Result : 'unsat' { Nothing }
  | Instance { Just $1 }

Instance : 'instance' Equations { Instance $2 }

Equations : Equations Equation { $2:$1 }
  | Equation { [$1] }

Equation : Path '=' '{' TuplesOrNothing '}' { Set $1 $4 }
  | Path '<:' Word '=' '{' TuplesOrNothing '}' { Relation $1 $3 $6 }
  | 'skolem' Word '=' '{' TuplesOrNothing '}' { Relation "Local" $2 $5 }

Path : Word { $1 } 
	| Path '/' Word { $1 ++ $3 }

Word : word { snd $1 }

TuplesOrNothing : Tuples { $1 }
  | { [] }

Tuples : Tuples ',' Tuple { (reverse $3):$1 }
  | Tuple { [reverse $1] }

Tuple : Tuple '->' Value { $3:$1 }
  | Value { [$1] }

Value : Path { $1 }
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
