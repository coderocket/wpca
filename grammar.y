{ 
module Grammar where
import Data.Char
import Lexer
-- import ParserMonad
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

%left 'and'
%nonassoc '=' '>=' '<=' '>' '<'
%left '+' '-' 
%left '*' '/' 'mod' 'div'
%%

Spec : Locals Pre ';' Seq Post { Spec $1 $2 $4 $5 }

Locals : LocalsList { Locals $1 }

LocalsList : Declaration { [$1] } 
	| LocalsList ';' Declaration { $3:$1 }

Declaration : Names ':' Expr { Declaration (reverse $1) $3 }

Names : name { [content $1] }
	| Names ',' name { (content $3):$1 }

Pre : '{' Expr '}' { $2 }

Post : '{' Expr '}' { $2 }

Seq : Stmt { $1 } 
	| Seq ';' Stmt { Seq $1 $3 }

Stmt : If { $1 }
    | Assign { $1 }
    | Do { $1 }
    | 'skip' { Skip }

Do : 'do' GuardedCommands 'od' { Loop (reverse $2) }

If : 'if' GuardedCommands 'fi' { Cond (reverse $2) }

GuardedCommands : Expr '->' Seq { [($1,$3)] }
    | GuardedCommands '[]' Expr '->' Seq { ($3,$5):$1 }

Assign : AssignOk  { Assign $1 }
    | AssignError { $1 }

AssignOk : name ':=' Expr { ([content $1], [$3]) }
	| name ',' AssignOk ',' Expr { join  (content $1) $3 $5 }

AssignError : AssignOk ',' {% failWithLoc $2 "assignment has more expressions than variables." }
    | name ',' AssignOk {% failWithLoc $2 "assignment has more variables than expressions." }

Expr : '-' Expr { Neg $2 }
	| Expr 'and' Expr { BinOp Conj $1 $3 }
	| Expr '>' Expr { BinOp NodeGreater $1 $3 }
	| Expr '<' Expr { BinOp NodeLess $1 $3 }
	| Expr '>=' Expr { BinOp NodeGeq $1 $3 }
	| Expr '<=' Expr { BinOp NodeLeq $1 $3 }
	| Expr '=' Expr { BinOp NodeEq $1 $3 }
	| Expr '+' Expr { BinOp Plus $1 $3 }
	| Expr '-' Expr { BinOp Minus $1 $3 }
	| Expr '*' Expr { BinOp Times $1 $3 }
	| Expr '/' Expr { BinOp Quotient $1 $3 }
	| Expr 'mod' Expr { BinOp Mod $1 $3 }
	| Expr 'div' Expr { BinOp Div $1 $3 }
	| Factor { $1 }

Factor: int { Nat (content $1) }
	| name { Var (content $1) }
	| 'int' { TypeVar "int" }
	| 'true' { PredTrue }
	| 'false' { PredFalse }
	| '(' Expr ')' { $2 }
	| Factor '[' ExprList ']' { foldr (BinOp Join) $1 $3 }

ExprList : Expr { [$1] }
	| ExprList ',' Expr { $3:$1 }
{

failWithLoc :: AlexPosn -> String -> Either String a
failWithLoc pos err = Left $ err ++ " at " ++ (show line) ++ " line, " ++ (show column) ++ " column\n"
    where
    getLineCol (AlexPn _ line col) = (line,col)
    (line, column) = getLineCol pos

parseError (t:tokens) = failWithLoc (pos t) "parse error\n"

parseError [] = Left "parse error at end of file\n"

join :: String -> ([String],[Node]) -> Node -> ([String],[Node])
join n (ns,es) e = (n:ns,es++[e])

parse :: String -> IO (Either String Node) 
parse s = return (hParse (alexScanTokens s)) 

}
