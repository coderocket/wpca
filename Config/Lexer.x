{
module Config.Lexer where
}

%wrapper "monad"

$digit = [0-9]
$alpha = [a-zA-Z]				-- alphabetic characters
$printable = [a-zA-Z0-9\/\\\.\;\-:]
@name = $alpha ($alpha | $digit)*
@string = \" ($printable | $white)* \" 

tokens :-

  $white+	{ skip }
  "--".*	{ skip }
  "="           { tok $ \p s -> TokEq p }
  ","           { tok $ \p s -> TokComma p }
  "["           { tok $ \p s -> TokLBra p }
  "]"           { tok $ \p s -> TokRBra p }
  @name		{ tok $ \p s -> TokName (p,s) }
  @string	{ tok $ \p s -> TokString (p,f s) }
  $printable+   { tok $ \p s -> TokString (p,s) }
{

f = (takeWhile (/= '"')).tail

tok t (pos, _, _,input) len = return (t pos (take len input))

alexEOF = return TokEOF

-- The token type:

data Token =
 	TokEOF |
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
