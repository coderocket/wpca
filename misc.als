open util/integer

module misc

fun max[x,y:Int]:Int {
  x < y => y else x
}

fun abs[x:Int]:Int { 
  x < 0 => 0.sub[x] else x
}

check { all x : Int | abs[abs[x]] = abs[x] }

check { all x : Int | x >= 0 => abs[x] = x }

run { some x,y : Int | x = abs[y] }
