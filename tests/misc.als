module misc

open util/integer

one sig NIL {}

sig Object {}

pred true { no none }
pred false { some none }

fun gcd[x,y:Int]:Int {
  max[cd[x,y]]
}

fun cd[x,y:Int]:set Int {
  { z : Int | rem[x,z] = 0 and rem[y,z] = 0  }  
}

fun max[x,y:Int]:Int {
  x < y => y else x
}

fun abs[x:Int]:Int { 
  x < 0 => 0.sub[x] else x
}

fun range[b,e:Int] : set Int {
  { z : Int | z.gte[b] and z.lte[e] }
}

pred permutation[b,e:Int,x,y : seq univ]
{
 	let r = range[b,e.sub[1]] |{
		#r<:x = #r<:y
		all t : Int.x | # (r<:x).t = # (r<:y).t 
	}
}

pred nochange[b,e:Int, x,X:seq univ] 
{
	let r = range[b,e.sub[1]] | {
		r <:x = r <: X
	}		
}

fun size[x:univ] : Int {
  #x
}

fun dres[s:univ, r:univ->univ] : univ->univ {
  s <: r
}

check { all x : Int | abs[abs[x]] = abs[x] }

check { all x : Int | x >= 0 => abs[x] = x }

run { some x,y : Int | x = abs[y] }
