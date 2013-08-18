
proc Swap[x,y : out int]
	{x = X and y = Y}
; x,y := y,x
	{x = Y and y = X}

proc Use[a : int]
	{ a = A }
; Swap[a,a]
	{ a = A }
