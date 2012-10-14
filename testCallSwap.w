
proc Swap[x,y : out int]
	{x = X and y = Y}
; x,y := y,x
	{x = Y and y = X}

proc Use[a,b : int]
	{ a = A and b = B }
; Swap[a,b]
	{ a = B and b = A }
