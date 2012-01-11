open util/integer

module misc

fun abs[x:Int]:Int { 
  x < 0 => 0.sub[x]  else x
}

check { all x : Int | abs[abs[x]] = abs[x] }

check { all x : Int | x >= 0 => abs[x] = x }

run { some x,y : Int | x = abs[y] }
