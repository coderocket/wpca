
proc P[x,y : out int]
	{x>0 and y>0}
; x := y-1
	{x < y}

proc Use[a : int]
	{ true }
; P[a,a]
	{ a = a}
