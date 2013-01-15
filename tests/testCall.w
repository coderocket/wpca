
proc Set[x : out int]
	{true}
; x := 1
	{x = 1}

proc Use[y : int]
	{ true }
; Set[y]
	{ y = 1 }
