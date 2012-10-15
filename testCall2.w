
proc Set[x,y : out int; z : int]
	{true}
; x := 1
	{x = 1}

proc Use[b : int]
  a : int
	{ true }
; Set[a,b]
	{ b = 1 }
