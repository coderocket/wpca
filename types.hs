
module Types where

{-

the types of the language are:

atom ::= int | sig <name> | univ | none

type ::= atom | rel atom ... atom

we define a subtype relation between the types as follows:

subtype = *dst

(we use dst to mean direct-subtype)

where

int dst univ
sig<name> dst univ
none dst t forall t in type

rel t1 ... tn dst rel t'1 ... t'n

if and only if ti dst t'i forall i :1..n


with each type we identify a set of entities. 

E[int] = the set of integers
E[sig<name>] = the set of <name> atoms
E[univ] = the set of all atoms in the world
E[rel t1...tn] = the set of all n-tuples (x1,...,xn) such that
xi in E[ti] forall i :1..n
E[none] = {}

we demand that the following theorm will hold:

t subtype t' iff E[t] in E[t']

we demand that dst has the following exclusive property:

if t1,...,tn are all the direct sub-types of t then

1. all i,j : 1..n | i != j => no E[ti] & E[tj]
2. E[t1]+E[t2]+...+E[tn] = E[t]

type expressions are expressions that represent sets (including
relations), that is, their value is a set of tuples. this includes
1-tuples, that is, sets of atoms. for example if u and v are
integers then u union v is the set {u,v}

V[u union v] = {u,v}

the type that corresponds to a type expression is the lowset type that
contains the type expression. that is,

T[e] = t iff V[e] in E[t] and all t' | V[e] in E[t'] => t subtype t'

type expression			type

type t				t
range				int
x + y				T[x] `max` T[y]
x & y				T[x] `min` T[y]
x - y 				T[x] `minus` T[y]

properties of minus:

0. t `minus` t' subset t
1. t `minus` none = t
2. none `minus` t = none
3. t `minus` t = none

this is a special case of 4:

4. t subset t' => t `minus` t' = none
5. not t subset t' and not t' subset t => t `minus` t' = t
6. t dst t' => t' `minus` t = max { x | x dst t' and x != t }

from (6) we see that: 

7. t1 and t2 are the only direct subtypes of t =>
     t - t1 = t2 and t - t2 = t1

8. t has more than two direct subtypes =>
     t - ti = t  

the pair of functions (T,E) form a Galois connection between the
domain of types and the domain of values. 

T : P(Value) -> Type
E : Type -> P(Value)

-}

type Name = String

data AtomType = INT | UNIV | Sig Name

data Type = Atom AtomType | Rel [AtomType]

subtype :: Type -> Type -> Bool

subtype (Atom INT) (Atom INT) = True
subtype (Atom INT) (Atom UNIV) = True
subtype (Atom UNIV) (Atom UNIV) = True
subtype (Atom (Sig n)) (Atom (Sig n')) = (n == n')
subtype (Atom (Sig _)) (Atom UNIV) = True
subtype (Rel ts) (Rel ts') = (length ts) == (length ts') && foldr1 (&&) [(Atom t) `subtype` (Atom t') | (t,t') <- zip ts ts']
subtype x y = False

