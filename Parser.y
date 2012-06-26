{ 
module Parser where
import Data.Char
import Lexer
import Data.Tree
import AST
import Loc
}

%name hParse
%tokentype { Token }
%error { parseError }
%monad { Either String } 
%token
	'true'	{ TokTrue $$ }
	'false'	{ TokFalse $$ }
	'int'	{ TokIntType $$ }
	'nat'	{ TokNatType $$ }
	'array'	{ TokArray $$ }
	'of'	{ TokOf $$ }
	int	{ TokInt $$ }
	name	{ TokName $$ }
	'+'	{ TokPlus $$ }
	'*'	{ TokStar $$ }
	'/'	{ TokSlash $$ }
	'-'	{ TokDash $$ }
	'('	{ TokLB $$ }
	')'	{ TokRB $$ }
	'{'	{ TokLCurl $$ }
	'}'	{ TokRCurl $$ }
	'['    	{ TokLSquare $$ }
	']'    	{ TokRSquare $$ }
	':='	{ TokAssign $$ }
	'='	{ TokEq $$ }
	'!='	{ TokNotEq $$ }
	'>'	{ TokGreater $$ }
	'<'	{ TokLess $$ }
	'>='	{ TokGeq $$ }
	'<='	{ TokLeq $$ }
	','	{ TokComma $$ }
	';'     { TokSemi $$ }
	':'     { TokColon $$ }
	'..'    { TokRange $$ }
	'.'    	{ TokDot $$ }
	'|'     { TokBar $$ }
	'and'   { TokAnd $$ }
	'or'   { TokOr $$ }
	'=>'   { TokImplies $$ }
	'do'    { TokDo $$ }
	'od'    { TokOd $$ }
	'if'    { TokIf $$ }
	'fi'    { TokFi $$ }
	'->'    { TokArrow $$ }
	'[]'    { TokSquare $$ }
	'mod'	{ TokMod $$ }
	'div'	{ TokDiv $$ }
	'skip'  { TokSkip $$ }
	'sum'  	{ TokSum $$ }
	'all'  	{ TokAll $$ }
	'some' 	{ TokSome $$ }
	'no'  	{ TokNo $$ }
	'keeping'  { TokKeep $$ }

%nonassoc ALL
%left 'and'
%left 'or'
%right '=>'
%nonassoc '=' '!=' '>=' '<=' '>' '<' 
%nonassoc '..' SUM
%left '+' '-' 
%left '*' '/' 'mod' 'div'
%left '.'
%%

Spec : Locals Pre ';' Seq Post { Node ($3,Spec) [$1, $2, $4, $5] }

Locals : LocalsList { Node (fst (rootLabel (head $1)), Locals) $1 }

LocalsList : Declaration { [$1] } 
	| LocalsList ';' Declaration { $3:$1 }

Declaration : Names ':' Expr { Node ($2, Declaration) [Node ($2, List) (reverse $1), $3] }

Names : name { [Node (fst $1, String (snd $1)) [] ] }
	| Names ',' name { (Node (fst $3, String (snd $3)) []):$1 }

Seq : Stmt { $1 } 
	| Seq ';' Stmt { Node ($2,Seq) [$1,$3] }

Stmt : If { $1 }
    | Assign { $1 }
    | Assert { $1 }
    | Do { $1 }
    | 'skip' { Node ($1,Skip) [] }

Do : DoOk { $1 }
	| DoError { $1 }

Assert : '{' Expr '}' { Node ($1, Assert) [$2] }

DoOk : 'keeping' Expr 'do' GuardedCommands 'od' { Node ($1,Loop) [$2, Node ($3, List) (reverse $4)] }

DoError : 'do' {% failWithLoc $1 "missing invariant clause." }

If : 'if' GuardedCommands 'fi' { Node ($1, Cond) (reverse $2) }

GuardedCommands : Expr '->' Seq { [Node ($2,List) [$1,$3]] }
    | GuardedCommands '[]' Expr '->' Seq { (Node ($2,List) [$3,$5]):$1 }

Assign : AssignOk { makeAssign $1 }
    | AssignError { $1 }

AssignOk : Term ':=' Term { ([$1], [$3]) }
	| Term ',' AssignOk ',' Term { join $1 $3 $5 }

AssignError : AssignOk ',' {% failWithLoc $2 "assignment has more expressions than variables." }
    | Term ',' AssignOk {% failWithLoc $2 "assignment has more variables than expressions." }

Pre : '{' Expr '}' { $2 }

Post : '{' Expr '}' { $2 }

Expr : Expr 'and' Expr { Node ($2,Conj) [$1, $3] }
	| Expr 'or' Expr { Node ($2,Disj) [$1, $3] }
	| Expr '=>' Expr { Node ($2,Implies) [$1, $3] }
	| Comprehension { $1 }
	| Relat { $1 }

Comprehension : 'sum' Locals '|' Expr %prec SUM { Node ($1, Quantifier Sum) [$2,$4] } 
	| 'all' Locals '|' Expr %prec ALL { Node ($1, Quantifier All) [$2,$4] } 
	| 'no' Locals '|' Expr %prec ALL { Node ($1, Quantifier No) [$2,$4] } 
	| 'some' Locals '|' Expr %prec ALL { Node ($1, Quantifier Some) [$2,$4] }

Relat :  Term '>' Term { Node ($2, Greater) [$1,$3] }
	| Term '<' Term { Node ($2, Less) [$1,$3] }
	| Term '>=' Term { Node ($2, Geq) [$1,$3] }
	| Term '<=' Term { Node ($2, Leq) [$1,$3] }
	| Term '=' Term { Node ($2, Eq) [$1,$3] }
	| Term '!=' Term { Node ($2, NotEq) [$1,$3] }
	| Term '<' Term '<' Term { (Node ($2, Less) [$1,$3]) `conj` (Node ($4, Less) [$3,$5]) }
	| Term '<=' Term '<' Term { (Node ($2, Leq) [$1,$3]) `conj` (Node ($4, Less) [$3,$5]) }
	| Term '<' Term '<=' Term { (Node ($2, Less) [$1,$3]) `conj` (Node ($4, Leq) [$3,$5]) }
	| Term '<=' Term '<=' Term { (Node ($2, Leq) [$1,$3]) `conj` (Node ($4, Leq) [$3,$5]) }
	| 'some' Term { Node ($1, SomeSet) [$2] }
	| Term { $1 }

Term: '-' Term { Node ($1, Neg) [$2] }
	| Term '+' Term { Node ($2, Plus) [$1,$3] }
	| Term '-' Term { Node ($2, Minus) [$1,$3] }
	| Term '*' Term { Node ($2, Times) [$1,$3] }
	| Term '/' Term { Node ($2, Quotient) [$1,$3] }
	| Term 'mod' Term { Node ($2, Mod) [$1,$3] }
	| Term 'div' Term { Node ($2, Div) [$1,$3] }
	| Term '..' Term { Node ($2, Range) [$1,$3] }
	| Term '.' Term { Node ($2, Join) [$1,$3] }
	| Factor { $1 }

Factor: Type { $1 }
	| int { Node (fst $1, Int (snd $1)) [] }
	| 'true' { Node ($1, AST.True) [] }
	| 'false' { Node ($1, AST.False) [] }
	| name { Node (fst $1, String (snd $1)) [] }
	| '(' Expr ')' { $2 }
	| Factor '[' TermList ']' { Node ($2, Join) ($3++[$1]) }

Type : BasicType { Node (fst $1, Type (snd $1)) [] } 
	| CompoundType { $1 }

BasicType : 'int' { ($1,"int") }
	| 'nat' { ($1,"nat") }

CompoundType : 'array' 'of' name BasicType { Node ($1, ArrayType (snd $3) (snd $4)) [] }
	| 'array' 'of' name name { Node ($1, ArrayType (snd $3) (snd $4)) [] }

TermList : Term { [$1] }
	| TermList ',' Term { $3:$1 }
{

makeAssign (names,exprs) = 
  Node (p ,Assign) [Node (p, List) names, Node (p, List) exprs]  
  where p = fst (rootLabel (head names)) 

failWithLoc :: Loc -> String -> Either String a
failWithLoc (line, col) err = Left $ err ++ " at " ++ (show line) ++ " line, " ++ (show col) ++ " column\n"

parseError (t:tokens) = failWithLoc (pos t) "parse error\n"

parseError [] = Left "parse error at end of file\n"

join :: a -> ([a],[b]) -> b -> ([a],[b])
join n (ns,es) e = (n:ns,es++[e])

parse :: String -> IO AST 
parse s = case (hParse (alexScanTokens s)) of
  (Left err) -> fail err
  (Right ast) -> return ast 

}

