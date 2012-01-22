{ 
module Grammar where
import Data.Char
import Lexer
import Data.Tree
import AST
}

%name hParse
%tokentype { Token }
%error { parseError }
%monad { Either String } 
%token
	'true'	{ TokTrue $$ }
	'false'	{ TokFalse $$ }
	'int'	{ TokIntType $$ }
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
	'['    { TokLSquare $$ }
	']'    { TokRSquare $$ }
	':='	{ TokAssign $$ }
	'='	{ TokEq $$ }
	'>'	{ TokGreater $$ }
	'<'	{ TokLess $$ }
	'>='	{ TokGeq $$ }
	'<='	{ TokLeq $$ }
	','	{ TokComma $$ }
	';'     { TokSemi $$ }
	':'     { TokColon $$ }
	'and'    { TokAnd $$ }
	'do'    { TokDo $$ }
	'od'    { TokOd $$ }
	'if'    { TokIf $$ }
	'fi'    { TokFi $$ }
	'->'    { TokArrow $$ }
	'[]'    { TokSquare $$ }
	'mod'	{ TokMod $$ }
	'div'	{ TokDiv $$ }
	'skip'  { TokSkip $$ }
	'keeping'  { TokKeep $$ }

%left 'and'
%nonassoc '=' '>=' '<=' '>' '<'
%left '+' '-' 
%left '*' '/' 'mod' 'div'
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
    | Do { $1 }
    | 'skip' { Node ($1,Skip) [] }

Do : 'keeping' Expr 'do' GuardedCommands 'od' { Node ($1,Loop) [$2, Node ($3, List) (reverse $4)] }

If : 'if' GuardedCommands 'fi' { Node ($1, Cond) (reverse $2) }

GuardedCommands : Expr '->' Seq { [Node ($2,List) [$1,$3]] }
    | GuardedCommands '[]' Expr '->' Seq { (Node ($2,List) [$3,$5]):$1 }

Assign : AssignOk { makeAssign $1 }
    | AssignError { $1 }

AssignOk : name ':=' Expr { ([Node (fst $1, String (snd $1)) []], [$3]) }
	| name ',' AssignOk ',' Expr { join (Node (fst $1, String (snd $1)) []) $3 $5 }

AssignError : AssignOk ',' {% failWithLoc $2 "assignment has more expressions than variables." }
    | name ',' AssignOk {% failWithLoc $2 "assignment has more variables than expressions." }

Pre : '{' Expr '}' { $2 }

Post : '{' Expr '}' { $2 }

Expr : '-' Expr { Node ($1, Neg) [$2] }
	| Expr 'and' Expr { Node ($2,Conj) [$1, $3] }
	| Expr '>' Expr { Node ($2, Greater) [$1,$3] }
	| Expr '<' Expr { Node ($2, Less) [$1,$3] }
	| Expr '>=' Expr { Node ($2, Geq) [$1,$3] }
	| Expr '<=' Expr { Node ($2, Leq) [$1,$3] }
	| Expr '=' Expr { Node ($2, Eq) [$1,$3] }
	| Expr '+' Expr { Node ($2, Plus) [$1,$3] }
	| Expr '-' Expr { Node ($2, Minus) [$1,$3] }
	| Expr '*' Expr { Node ($2, Times) [$1,$3] }
	| Expr '/' Expr { Node ($2, Quotient) [$1,$3] }
	| Expr 'mod' Expr { Node ($2, Mod) [$1,$3] }
	| Expr 'div' Expr { Node ($2, Div) [$1,$3] }
	| Factor { $1 }

Factor: int { Node (fst $1, Int (snd $1)) [] }
	| name { Node (fst $1, String (snd $1)) [] }
	| 'int' { Node ($1, Type "int") [] }
	| 'true' { Node ($1, AST.True) [] }
	| 'false' { Node ($1, AST.False) [] }
	| '(' Expr ')' { $2 }
	| Factor '[' ExprList ']' { Node ($2, Join) ($3++[$1]) }

ExprList : Expr { [$1] }
	| ExprList ',' Expr { $3:$1 }
{

makeAssign (names,exprs) = 
  Node (p ,Assign) [Node (p, List) names, Node (p, List) exprs]  
  where p = fst (rootLabel (head names)) 

failWithLoc :: AlexPosn -> String -> Either String a
failWithLoc pos err = Left $ err ++ " at " ++ (show line) ++ " line, " ++ (show column) ++ " column\n"
    where
    getLineCol (AlexPn _ line col) = (line,col)
    (line, column) = getLineCol pos

parseError (t:tokens) = failWithLoc (pos t) "parse error\n"

parseError [] = Left "parse error at end of file\n"

join :: a -> ([a],[b]) -> b -> ([a],[b])
join n (ns,es) e = (n:ns,es++[e])

parse :: String -> IO (Either String AST) 
parse s = return (hParse (alexScanTokens s)) 

}

