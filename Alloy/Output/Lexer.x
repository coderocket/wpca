{
module Alloy.Output.Lexer where
}

%wrapper "monad"

$digit = [0-9]
$alpha = [a-zA-Z_]			-- alphabetic characters
@num = \-? $digit +
@word = ($alpha | "$") ($alpha | "$" | $digit)* 

tokens :-

  $white+	{ skip }
  "---OUTCOME---"	{ skip }
  ":"           { tok $ \p s -> TokColon p }
  "<:"          { tok $ \p s -> TokRestrict p }
  "->"          { tok $ \p s -> TokArrow p }
  "="           { tok $ \p s -> TokEq p }
  ","           { tok $ \p s -> TokComma p }
  "/"           { tok $ \p s -> TokSlash p }
  "{"           { tok $ \p s -> TokLCurl p }
  "}"           { tok $ \p s -> TokRCurl p }
  "Check"	{ tok $ \p s -> TokCheck p }
  "this"	{ tok $ \p s -> TokThis p }
  "skolem"	{ tok $ \p s -> TokSkolem p }
  "---INSTANCE---"	{ tok $ \p s -> TokInstance p }
  "Unsatisfiable."	{ tok $ \p s -> TokUnsat p }
  @word		{ tok $ \p s -> TokWord (p,s) }
  @num		{ tok $ \p s -> TokNumber (p,read s) }
{

f = (takeWhile (/= '"')).tail

tok t (pos, _, input) len = return (t pos (take len input))

alexEOF = return TokEOF

-- The token type:

data Token =
 	TokEOF |
	TokColon AlexPosn |
	TokRestrict AlexPosn |
	TokArrow AlexPosn |
	TokEq AlexPosn |
	TokComma AlexPosn |
	TokSlash AlexPosn |
	TokLCurl AlexPosn |
	TokRCurl AlexPosn |
	TokThis AlexPosn |
	TokSkolem AlexPosn |
	TokInstance AlexPosn |
	TokCheck AlexPosn |
	TokUnsat AlexPosn |
	TokWord (AlexPosn,String) |
	TokNumber (AlexPosn,Int) 
	deriving (Eq,Show)

pos (TokColon p) = p
pos (TokRestrict p) = p
pos (TokArrow p) = p
pos (TokEq p) = p
pos (TokComma p) = p
pos (TokUnsat p) = p
pos (TokCheck p) = p
pos (TokSlash p) = p
pos (TokLCurl p) = p
pos (TokRCurl p) = p
pos (TokThis p) = p
pos (TokSkolem p) = p
pos (TokWord (p, _)) = p
pos (TokNumber (p, _)) = p

content (p,v) = v

}
