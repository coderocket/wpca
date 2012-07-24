module CLang.Generator where
import List
import AST
import Data.Tree
import LookupMonad
import Typechecker
import Loc

{-

A WPCA program consists of a set of record definitions followed by 
a set of procedure definitions. To generate a C program we have to
create two files: a header file and a source file. The header file
will contain all the structs that correspond to the records. and the
prototypes of all the functions that correspond to the procedures. The 
source file will include the header file and define all the functions.

-}

type ConfigEnv = [(String,[String])]

generate :: ConfigEnv -> AST -> IO ()
generate env program = 
  do generateHeader env program 
     generateSource env program

generateHeader env program = 
  do hs <- lookupM "c.headerfile" env
     putStrLn ("Writing c header file to " ++ (head hs))
     writeFile (head hs) (prepareHeader program)

generateSource env program =
  do cs <- lookupM "c.sourcefile" env
     hs <- lookupM "c.headerfile" env
     putStrLn ("Writing c source file to " ++ (head cs))
     writeFile (head cs) (prepareSource (head cs) program)

prepareHeader :: AST -> String
prepareHeader (Node (_,Program) [Node (_,List) records, Node (_,List) procs, theory]) =
  header ++ prepareStructs records ++ preparePrototypes procs ++ footer
  where header = ""  
        footer = ""

prepareStructs :: [AST] -> String
prepareStructs = foldr (++) "" . map prepareStruct

prepareStruct :: AST -> String
prepareStruct (Node (_,Record name) fields) = header ++ (prepareFields (declsToList fields)) ++ footer
  where header = "typedef struct _" ++ name ++ " {\n"
        footer = "} " ++ name ++ ";\n"

prepareFields = foldr (++) "" . map prepareField
prepareField :: (String,AST) -> String
prepareField (name,tp) = "" ++ " " ++ name ++ ";"

preparePrototypes :: [AST] -> String
preparePrototypes = foldr (++) "" . map (appendSemi . preparePrototype)
  where appendSemi s = s ++ ";\n"

preparePrototype :: AST -> String
preparePrototype (Node (_,Proc name) [Node (_,Locals) params,pre,body,post]) = 
  "void " ++ name ++ "(" ++ prepareParams params ++ ")"

prepareParams :: [AST] -> String
prepareParams = foldr sepWithComma "" . map prepareParam . declsToList
  where sepWithComma s "" = s
        sepWithComma s rest = s ++ ", " ++ rest

prepareParam :: (String, AST) -> String
prepareParam (name,typ) = (showCode typ) ++ " " ++ name

-- TODO: add the pre and post conditions as documentation above the prototype
 
prepareSource :: String -> AST -> String
prepareSource hfile (Node (_,Program) [Node (_,List) records, Node (_,List) procs, theory]) = header ++ prepareFunctions procs ++ footer
  where header = "#include \"" ++ hfile ++ "\"\n"
        footer = ""

prepareFunctions :: [AST] -> String
prepareFunctions = foldr (++) "" . map prepareFunction

prepareFunction :: AST -> String
prepareFunction proc =
  preparePrototype proc ++ "{\n" ++ prepareBody proc ++ "}\n"

{-
To prepare the body of a procedure we first normalize(see below) it 
and then annotate its variables so that we will know their type when
without having to lookup an environment. Finally we convert the 
annotated program into its C equivalent program.

By normalizing a program we mean that we transform it into a form
that resembles traditional programming language. This includes:

1. Replace multiple assignment statements with a sequence of
 simple assignment statements
2. Convert do .. od loops into a combination of while and 
simple if statements 
3. Convert if .. fi statements into simple if/else statements.  

-}

prepareBody :: AST -> String
prepareBody = showCode . annotate . normalize 

normalize :: AST -> (AST,AST)
normalize (Node (_,Proc name) [Node (_,Locals) params,pre,body,post]) = 
  transform (declsToList params) body

annotate :: (AST, AST) -> AST
annotate (locals,normalized) = locals `wseq` normalized

showCode :: AST -> String

showCode = foldRose f 
  where f (_,Seq) xs = foldr (\ x rest -> x ++ "\n" ++ rest) "" xs 
        f (_,Skip) [] = ";"
        f (_,Break) [] = "break;"
        f (_,Type "nat") [] = "unsigned"
        f (_,Type n) [] = n
        f (_,ArrayType _ t) [] = t ++ "*"
        f (_,Join) xs = (showJoin xs)
        f (_,ArrayJoin) xs = (showArrayJoin xs)
        f (_,String n) [] = n 
        f (_,Int x) [] = (show x)
        f (_,Neg) [x] = "-" ++ x
        f (_,Plus) [x,y] = "(" ++ x ++ "+" ++ y ++ ")"
        f (_,Minus) [x,y] = "(" ++ x ++ "-" ++ y ++ ")"
        f (_,Times) [x,y] = x ++ "*" ++ y 
        f (_,Quotient) [x,y] = x ++ " / " ++ y
        f (_,Div) [x,y] = x ++ " / " ++ y 
        f (_,Mod) [x,y] = x ++ " % " ++ y
        f (_,Eq) [x,y] = x ++ " == " ++ y
        f (_,NotEq) [x,y] = x ++ " != " ++ y
        f (_,Greater) [x,y] = x ++ " > " ++ y
        f (_,Less) [x,y] = x ++ " < " ++ y
        f (_,Geq) [x,y] = x ++ " >= " ++ y
        f (_,Leq) [x,y] = x ++ " <= " ++ y
        f (_,Conj) [x,y] = x ++ " && " ++ y
        f (_,Disj) [x,y] = "("++ x ++ " || " ++ y ++ ")"
        f (_,Implies) [x,y] = "(!(" ++ x ++ ") || " ++ y ++")"
        f (_,AST.True) [] = "1"
        f (_,AST.False) [] = "0"
        f (_,Not) [x] = "!(" ++ x ++ ")"
        f (_,Assign) [n,e] = n ++ " = " ++ e ++ ";"
        f (_,Assert) [_] = ";"
        f (_,List) xs = foldr (++) "" xs
        f (_,Cond) [g,x,y] = "if (" ++ g ++ ") {\n" ++ x ++ "\n} else {\n" ++ y ++"}\n"
        f (_,Loop) [gs] = "while(1) {\n" ++ gs ++ "\n}\n"
        f (_,Locals) xs = foldr (++) "" xs
	f (_,Declaration) [n, t] = t ++ " " ++ n ++ ";\n"
        f (_,x) xs = error ("C does not support " ++ show x)

-- [x,y,z] => x->y->z

showJoin = foldr f ""
  where f x [] = x
        f x xs = x ++ "->" ++  xs  

-- a.b b[a]
-- a.b.c (b.c)[a] c[b][a]
-- a b c  => [a] [b] c => c [b] [a] => c[b][a]

showArrayJoin = (foldr (++) "") . reverse . (foldr f [])
  where f x [] = [x]
        f x xs = ("[" ++ x ++ "]") :  xs  

showLocalsX = foldr (++) "" . map showLocalX

showLocalX (Node (_,Declaration) [(Node (_,List) ds), t]) =
 foldr (++) "" [ showCode t ++ " " ++ name ++ ";\n" | Node (_,String name) [] <- ds ]
 
showLocals (Node (_,Locals) ds) = foldr f "" (declsToList ds) 
  where f (n,t) ds = case t of 
                        Node (_,Product) _ -> ds 
			Node (_,String s) [] -> s ++ " *" ++ n ++ ";\n" ++ ds
                        otherwise -> (showCode t) ++ " " ++ n ++ ";\n" ++ ds

{-

The problem: C does not support multiple assignment. Therefore we need
to transform assignment statements such as

x1,x2,...,xn := e1,e2,...,en


where xi is an L-value (a variable or an array cell)

into a sequence of n single assignment statements:

  y1 := e'1
; y2 := e'2
...
; ym := e'm

This sequence of assignment statements must have the same meaning as
the multiple assignmnet statement.

The unknown is therefore an algorithm that transforms a wpca program
into an equivalent wpca program that uses only single assignments.

Let us look again at the multiple assignment statement. If the expressions
do not contain free occurences of the L-values to which we assign, then
they do not depend on these L-values. This means that we can assign into the
L-values in any order.

However, if an expression ei has a free occurence of variable xj then
we must not assign to xj before we have evaluated ei. This approach will
work as long as there are no circular dependencies between the different
expressions. But consider the following assignment:

x,y := y,x

We cannot assign into y because it is needed by the first expression and
we cannot assign into x because it is needed by the second expression.

Alternatively we may keep the value of ei in a new temporary variable
xj_new and assign the new variable to the L-value only after we have
evaluated all the expressions. To implement this technique we will
need to:

1. Create temporary variables for each L-value. This is easier for
variables than for array expressions.

2. Assign the expressions in the multiple assignment statement to the
new variables.

4. Assign the new variables to their corresponding L-values

For example:

x,y,A[i+1] := y+x,x,A[j]+x

becomes

  x_1 := y+x
; y_1 := x
; array_A_1 := A[j]+x
; x := x_1
; y := y_1
; A[i+1] := array_A_1

or

  tmp_1 := y+x
; tmp_2 := x
; tmp_3 := A[j]+x
; x := tmp_1
; y := tmp_2
; A[i+1] := tmp_3

We now discuss how we may implement each of these procedures:

1. Create temporary variables for each L-value. 

There are three problems here:

1.1 How do we know what type to give to the new variable?

The type of the variable is that of the L-value. To find the type of
the L-value we need a function that calculates the type of an expression
given the types of its free variables.

1.2 How do we create a name for the variable without colliding with an
existing variable name?

- If the L-value is a simple variable then we may use the following algorithm:

Add the suffix "_1" to the original variable and check if the new name
occurs free in the program. Keep incrementing the counter until the name
does not occur free in the program.

- If the L-value is an array expression of the form a[expr] then:

Create the name array_a_1 and see if the name occurs free in the
program. Keep incrementing the counter until the name does not occur
free in the program.

1.3 How do we associate the name of the temporary variable with the L-value?

 - There are many assignment statements in the program.
 - Because we have arrays there is no longer a simple connection
between the state variable declarations and the L-values that may appear
in the assignment statements.
 - Because we might capture existing variables we cannot use the name
in the L-value to generate the corresponding temporary variable's name.

We can use a two pass algorithm:

1. Transform the assignments one by one, pulling temporary variable
names from a stream of names.

2. Find all the free variables that are L-values in assignments and add
declarations for them.

This means that we will also define variables that were not originaly
defined by the user. This will prevent the compiler from flagging them
as errors.

An alternative is to keep a set of names while we transform the
assignments. Then return both the transformed program and the set of names
and finally add the names in the set to the definitions of the program.

The function tassign takes as input: an AST and a set of unused names,
and returns three entities: the transformed AST, a set of unused names
and an environment that maps each used name to its type.

-}

newNames = [ "tmp_" ++ (show i) | i <- [0..] ]

tassign :: Env -> AST -> [String] -> (AST, [String], Env)

tassign env (Node (pos,Assign) [Node (_,List) (x:y:ns), Node (_,List) es]) unused = 
 (tnode, newUnused, zip used types)
 where used = take (2 + (length ns)) unused
       newUnused = drop (2 + (length ns)) unused
       types = [ typeof env e | e <- es ]
       tnode = (assignUsed (zip used es)) `wseq` (assignLValues (zip (x:y:ns) used))

tassign env (Node h ns) unused = (Node h tnodes, newUnused, newUsed)
  where (tnodes, newUnused, newUsed) = tassignNodes env ns unused

tassignNodes :: Env -> [AST] -> [String] -> ([AST], [String], Env)

tassignNodes env ns unused = foldr f ([], unused, []) ns
  where f n (tnodes, newUnused, newUsed) = (tnode:tnodes, newNewUnUsed, newUsed ++ newNewUsed)
          where (tnode, newNewUnUsed, newNewUsed) = tassign env n newUnused
        
assignUsed :: [(String, AST)] -> AST
assignUsed as = foldr wseq skip [ assign n e | (n,e) <- as ]

assignLValues :: [(AST, String)] -> AST
assignLValues as = foldr wseq skip [ assignLvalue lvalue (string n) | (lvalue,n) <- as ]
  where assignLvalue lvalue e = Node (fst (rootLabel e), Assign) [lvalue, e]

{-

To transform the entire program we first transform the assignment
statements and add the new used variables to the declaration list. We
then transform the guards in the program (both in 'if' statements and
in loops).  

-}

transform :: Env -> AST -> (AST, AST)
transform stateVars program = (locals, tprog)
  where (tprog, _, used) = tassign stateVars (transformGuards program) newNames
        locals = Node (startLoc, Locals) (listToDecls used) 

listToDecls :: Env -> [AST]
listToDecls env = [ Node (startLoc, Declaration) [string n, t] | (n,t) <- env ]

transformGuards :: AST -> AST

transformGuards = foldRose f
  where f (_,Cond) gs = transformCond gs
        f (pos,Loop) [_,(Node (_,List) gs)] = Node (pos, Loop) [transformLoop gs]
        f (pos,kind) xs = Node (pos,kind) xs

transformCond = foldr f skip
  where f (Node (pos,List) [g,s]) gs = Node (pos,Cond) [g,s,gs] 

transformLoop = foldr f AST.break
  where f (Node (pos,List) [g,s]) gs = Node (pos,Cond) [g,s,gs]

