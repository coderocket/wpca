open util/integer

module misc

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

check { all x : Int | abs[abs[x]] = abs[x] }

check { all x : Int | x >= 0 => abs[x] = x }

run { some x,y : Int | x = abs[y] }
