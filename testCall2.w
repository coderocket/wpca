
proc Set[x,y : out int, z : int]
	{true}
; x := 1
	{x = 1}

proc Use[b : int]
	{ true }
  a : int
; Set[a,b,a]
	{ b = 1 }
