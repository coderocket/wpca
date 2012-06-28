module chain[T]

pred chain[r:T -> T] {
	r in T lone -> lone T
	one head : T | {
		no r.head
		T.r in head.*r
	}
}

fun maximal[r:T->T] : T {
	{ e : T | e in T.^r and no e.r }
}

fun minimal[r:T->T] : T {
	{ e : T | e in ^r.T and no r.e }
}

check {
	all e : T,next:T->T | e = maximal[next] iff e = minimal[~next]
}


fun concat[x,y:T->T] : T -> T {
	x + (maximal[x] -> minimal[y]) + y
}

check { 
	all next1,next2,next3 : T -> T |
	(chain[next1] and chain[next2] and chain[next3] and disjoint[links[next1],links[next2],links[next3]]) => 
		concat[concat[next1,next2],next3] = concat[next1,concat[next2,next3]]
}

check {
  all x,y : T->T | no x and no y => no concat[x,y] 
}

fun links[x:T->T] : set T { T.x + x.T }

check {
  all next : T -> T | chain[next] => no ^next & iden
}

fun crange[b,e:T, next:T->T] : set T {
  { p : T | p in b.*next and e in p.^next }
}

fun subchain[b,e:T, next:T->T] : T -> T {
	crange[b,e,next] <: next :> crange[b,e,next]
}

