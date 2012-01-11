{
module CFGLexer where
}

%wrapper "posn"

$digit = [0-9]
$alpha = [a-zA]				-- alphabetic characters
$printable = [a-zA-Z0-9\/\\\.]		

tokens :-

  $white+				;
  "--".*				;
  "="                                   { \p s -> TokEq p }
  ","                                   { \p s -> TokComma p }
  "["                                   { \p s -> TokLBra p }
  "]"                                   { \p s -> TokRBra p }
  $alpha [$alpha $digit]*		{ \p s -> TokName (p,s) }
  $printable+ 				{ \p s -> TokString (p,s) }
{

-- The token type:

data Token =
	TokLBra AlexPosn |
	TokRBra AlexPosn |
	TokEq AlexPosn |
	TokComma AlexPosn |
	TokName (AlexPosn,String) |
	TokString (AlexPosn,String) 
	deriving (Eq,Show)

pos (TokLBra p) = p
pos (TokRBra p) = p
pos (TokEq p) = p
pos (TokComma p) = p
pos (TokName (p, _)) = p
pos (TokString (p, _)) = p

content (p,v) = v

}
