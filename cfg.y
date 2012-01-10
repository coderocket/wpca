{ 
module CFGGrammar where
import Data.Char
import CFGLexer
import ParserMonad
}

%name hParse
%tokentype { Token }
%error { parseError }
%monad { P } 
%token
	name	{ TokName $$ }
	string	{ TokString $$ }
	'='	{ TokEq $$ }
	','	{ TokComma $$ }
	'['    { TokLBra $$ }
	']'    { TokRBra $$ }

%%

Config : Section { [$1] } 
  | Config Section { $2:$1 }

Section : '[' name ']' Vars { Section (content $2) $4 }

Vars : Var { [$1] }
  | Vars Var { $2:$1 }

Var : Names '=' Values { (foldr (++) "" (reverse $1),$3) }

Names : name { [content $1] }
  | Names name { (content $2) : $1 }

Values : Value { [$1] }
  | Values ',' Value { $3:$1 }

Value : string { content $1 }
  | name { content $1 }
{

data Section = Section String [(String,[String])]
	deriving (Show)

sectionToEnv :: Section -> [(String,[String])]
sectionToEnv (Section sname ds) = foldr f [] ds 
  where f (s,vs) ds = (sname++"."++s,vs):ds
  
content (p, v) = v

failWithLoc :: AlexPosn -> String -> P a
failWithLoc pos err = Failed $ err ++ " at " ++ (show line) ++ " line, " ++ (show column) ++ " column\n"
    where
    getLineCol (AlexPn _ line col) = (line,col)
    (line, column) = getLineCol pos

parseError (t:tokens) = failWithLoc (pos t) "parse error\n"

parseError [] = failP "parse error at end of file\n"

parseConfig s = do sec <- hParse (alexScanTokens s)  
                   return (foldr (++) [] (map sectionToEnv sec))
}
