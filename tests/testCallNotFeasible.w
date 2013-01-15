
proc P[x,y : out int]
	{x < y}
; x := y
	{x = y}

proc Use[a : int]
	{ true }
; P[a,a]
	{ a = a}
