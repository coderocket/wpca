
module Fresh where

type Name = String

{-

fresh old existing 

Takes a set of $n $old names and a set of $existing names and returns
a set of $n new names that do not appear in either $old or $existing:

no new & (old + existing) and #old = #new

where new = fresh old existing

To prove this theorem we will use the following lemma:

assuming that:

1. $freshName is correct 
2. no new & existing
3. old in existing 

under these assumptions we argue that: 

no (old+existing & res) and #res = #old + #new

where res = loop old existing new

We can use this lemma to prove the theorem like that:

fresh old existing 
= loop old (old+existing) []

{ (2) and (3) are satisfied }

=> no (old+existing & res) and #res = #old +#[] = #old

QED

We now prove the lemma:

1. To prove the base case we argue as follows:

loop [] existing new 
= new

and of course the lemma holds

2. To prove the induction step we argue as follows:

loop (n:ns) existing new
= loop ns existing n':new
  
where n' = freshName n (n:ns)+existing+new

(2) is satisfied for $existing and $new because of the initial assumption
and because $n' is not in $new.

(3) is satisfied because of the initial assumption and the fact that we
have removed an element from $old. Therefore, by induction we claim that

no (ns+existing & res) and #res = #ns + #(n':new)

where res = loop ns existing n':new

= { $n' is not in $new because of (1) }

no (ns+existing & res) and #res = #ns + 1 + #new

= { $n$ is not in res because of (3) }

no (n:ns+existing & res) and #res = #(n:ns) + #new
 
QED

-}

fresh :: [Name] -> [Name] -> [Name]
fresh old existing = loop old (old++existing) [] 
  where loop [] _ new = new
        loop (n:ns) existing new = loop ns existing (n':new)
          where n' = freshName n (new++existing)

{-

freshName old existing

Takes an $old name and a set of $existing names and returns a new name
that does not appear in $existing.

no freshName old existing & (old + existing)

-}

freshName :: Name -> [Name] -> Name
freshName candidate ns = 
  if candidate `elem` ns 
  then freshName (candidate++"'") ns
  else candidate


