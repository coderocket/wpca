{
module Lexer where
}

%wrapper "posn"

$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters

tokens :-

  $white+				;
  "--".*				;
  ">"                                   { \p s -> TokGreater p }
  ">="                                   { \p s -> TokGeq p }
  "<="                                   { \p s -> TokLeq p }
  "<"                                   { \p s -> TokLess p }
  "="                                   { \p s -> TokEq p }
  ":="                                  { \p s -> TokAssign p }
  ","                                   { \p s -> TokComma p }
  ";"                                   { \p s -> TokSemi p }
  ":"                                   { \p s -> TokColon p }
  "[]"                                  { \p s -> TokSquare p }
  "["                                  { \p s -> TokLSquare p }
  "]"                                  { \p s -> TokRSquare p }
  "->"                                  { \p s -> TokArrow p }
  "("                                   { \p s -> TokLB p }
  ")"                                   { \p s -> TokRB p }
  "{"                                   { \p s -> TokLCurl p }
  "}"                                   { \p s -> TokRCurl p }
  "+"                                   { \p s -> TokPlus p }
  "-"                                   { \p s -> TokDash p }
  "*"                                   { \p s -> TokStar p }
  "/"                                   { \p s -> TokSlash p }
  "true"                                { \p s -> TokTrue p }
  "false"                               { \p s -> TokFalse p }
  "int"                                 { \p s -> TokIntType p }
  "and"                                 { \p s -> TokAnd p }
  "div"                                 { \p s -> TokDiv p }
  "mod"                                 { \p s -> TokMod p }
  "od"                                  { \p s -> TokOd p }
  "od"                                  { \p s -> TokOd p }
  "if"                                  { \p s -> TokIf p }
  "fi"                                  { \p s -> TokFi p }
  "skip"                                { \p s -> TokSkip p }
  $alpha [$alpha $digit]*		{ \p s -> TokName (p,s) }
  $digit+				{ \p s -> TokInt (p,(read s)) }
{

-- The token type:

data Token =
	TokTrue AlexPosn |
	TokFalse AlexPosn |
	TokGreater AlexPosn |
	TokLess AlexPosn |
	TokGeq AlexPosn |
	TokLeq AlexPosn |
	TokIntType AlexPosn |
	TokAnd AlexPosn |
	TokPlus AlexPosn |
	TokStar AlexPosn |
	TokDash AlexPosn |
	TokSlash AlexPosn |
	TokDiv AlexPosn |
	TokMod AlexPosn |
	TokLB AlexPosn |
	TokRB AlexPosn |
	TokLCurl AlexPosn |
	TokRCurl AlexPosn |
	TokEq AlexPosn |
	TokAssign AlexPosn |
	TokComma AlexPosn |
	TokName (AlexPosn,String) |
	TokInt (AlexPosn,Int) |
        TokSemi AlexPosn |
        TokColon AlexPosn |
	TokDo AlexPosn |
	TokOd AlexPosn |
	TokIf AlexPosn |
	TokFi AlexPosn |
	TokArrow AlexPosn |
	TokLSquare AlexPosn |
	TokRSquare AlexPosn |
	TokSquare AlexPosn |
	TokSkip AlexPosn
	deriving (Eq,Show)

pos (TokTrue p) = p
pos (TokFalse p) = p
pos (TokGreater p) = p
pos (TokLess p) = p
pos (TokGeq p) = p
pos (TokLeq p) = p
pos (TokIntType p) = p
pos (TokAnd p) = p
pos (TokPlus p) = p
pos (TokStar p) = p
pos (TokDash p) = p
pos (TokSlash p) = p
pos (TokDiv p) = p
pos (TokMod p) = p
pos (TokLB p) = p
pos (TokRB p) = p
pos (TokLCurl p) = p
pos (TokRCurl p) = p
pos (TokAssign p) = p
pos (TokEq p) = p
pos (TokComma p) = p
pos (TokName (p, _)) = p
pos (TokInt (p,_)) = p
pos (TokSemi p) = p
pos (TokColon p) = p
pos (TokDo p) = p
pos (TokOd  p) = p
pos (TokIf  p) = p
pos (TokFi p) = p
pos (TokArrow p) = p
pos (TokLSquare p) = p
pos (TokRSquare p) = p
pos (TokSquare p) = p
pos (TokSkip p) = p

content (p,v) = v

}
