{
module Lexer where
import Loc
}

%wrapper "posn"

$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters

tokens :-

  $white+				;
  "--".*				;
  ">"                                   { \p s -> TokGreater (loc p) }
  ">="                                   { \p s -> TokGeq (loc p) }
  "<="                                   { \p s -> TokLeq (loc p) }
  "<"                                   { \p s -> TokLess (loc p) }
  "="                                   { \p s -> TokEq (loc p) }
  "!="                                   { \p s -> TokNotEq (loc p) }
  ":="                                  { \p s -> TokAssign (loc p) }
  ","                                   { \p s -> TokComma (loc p) }
  ";"                                   { \p s -> TokSemi (loc p) }
  ".."                                   { \p s -> TokRange (loc p) }
  ":"                                   { \p s -> TokColon (loc p) }
  "[]"                                  { \p s -> TokSquare (loc p) }
  "["                                  { \p s -> TokLSquare (loc p) }
  "]"                                  { \p s -> TokRSquare (loc p) }
  "->"                                  { \p s -> TokArrow (loc p) }
  "("                                   { \p s -> TokLB (loc p) }
  ")"                                   { \p s -> TokRB (loc p) }
  "{"                                   { \p s -> TokLCurl (loc p) }
  "}"                                   { \p s -> TokRCurl (loc p) }
  "+"                                   { \p s -> TokPlus (loc p) }
  "-"                                   { \p s -> TokDash (loc p) }
  "*"                                   { \p s -> TokStar (loc p) }
  "/"                                   { \p s -> TokSlash (loc p) }
  "|"                                   { \p s -> TokBar (loc p) }
  "true"                                { \p s -> TokTrue (loc p) }
  "false"                               { \p s -> TokFalse (loc p) }
  "int"                                 { \p s -> TokIntType (loc p) }
  "nat"                                 { \p s -> TokNatType (loc p) }
  "and"                                 { \p s -> TokAnd (loc p) }
  "or"                                 { \p s -> TokOr (loc p) }
  "=>"                                 { \p s -> TokImplies (loc p) }
  "div"                                 { \p s -> TokDiv (loc p) }
  "mod"                                 { \p s -> TokMod (loc p) }
  "do"                                  { \p s -> TokDo (loc p) }
  "od"                                  { \p s -> TokOd (loc p) }
  "if"                                  { \p s -> TokIf (loc p) }
  "fi"                                  { \p s -> TokFi (loc p) }
  "skip"                                { \p s -> TokSkip (loc p) }
  "sum"                                 { \p s -> TokSum (loc p) }
  "all"                                 { \p s -> TokAll (loc p) }
  "no"                                 { \p s -> TokNo (loc p) }
  "array"                               { \p s -> TokArray (loc p) }
  "of"                                  { \p s -> TokOf (loc p) }
  "keeping"                             { \p s -> TokKeep (loc p) }
  $alpha [$alpha $digit]*		{ \p s -> TokName (loc p,s) }
  $digit+				{ \p s -> TokInt (loc p,(read s)) }
{

loc :: AlexPosn -> Loc
loc (AlexPn _ line col) = (line,col)

-- The token type:

data Token =
	TokRange Loc |
	TokTrue Loc |
	TokFalse Loc |
	TokGreater Loc |
	TokLess Loc |
	TokGeq Loc |
	TokLeq Loc |
	TokIntType Loc |
	TokNatType Loc |
	TokAnd Loc |
	TokOr Loc |
	TokImplies Loc |
	TokPlus Loc |
	TokStar Loc |
	TokDash Loc |
	TokSlash Loc |
	TokDiv Loc |
	TokMod Loc |
	TokLB Loc |
	TokRB Loc |
	TokLCurl Loc |
	TokRCurl Loc |
	TokEq Loc |
	TokNotEq Loc |
	TokAssign Loc |
	TokComma Loc |
	TokName (Loc,String) |
	TokInt (Loc,Int) |
        TokSemi Loc |
        TokColon Loc |
	TokDo Loc |
	TokOd Loc |
	TokIf Loc |
	TokFi Loc |
	TokOf Loc |
	TokArray Loc |
	TokArrow Loc |
	TokLSquare Loc |
	TokRSquare Loc |
	TokSquare Loc |
	TokSkip Loc |
	TokSum Loc |
	TokAll Loc |
	TokNo Loc |
	TokBar Loc |
	TokKeep Loc
	deriving (Eq,Show)

pos (TokRange p) = p
pos (TokTrue p) = p
pos (TokFalse p) = p
pos (TokGreater p) = p
pos (TokLess p) = p
pos (TokGeq p) = p
pos (TokLeq p) = p
pos (TokIntType p) = p
pos (TokNatType p) = p
pos (TokAnd p) = p
pos (TokOr p) = p
pos (TokImplies p) = p
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
pos (TokNotEq p) = p
pos (TokComma p) = p
pos (TokName (p, _)) = p
pos (TokInt (p,_)) = p
pos (TokSemi p) = p
pos (TokColon p) = p
pos (TokDo p) = p
pos (TokOd  p) = p
pos (TokIf  p) = p
pos (TokFi p) = p
pos (TokOf p) = p
pos (TokArray p) = p
pos (TokArrow p) = p
pos (TokLSquare p) = p
pos (TokRSquare p) = p
pos (TokSquare p) = p
pos (TokSkip p) = p
pos (TokSum p) = p
pos (TokAll p) = p
pos (TokNo p) = p
pos (TokBar p) = p
pos (TokKeep p) = p

content (p,v) = v

showPos :: AlexPosn -> String
showPos (AlexPn addr line col) = (show line) ++ "_" ++ (show col)

}
