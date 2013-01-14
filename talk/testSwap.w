proc Swap[x, y : out int]
	{x = X and y = Y}
  t : int
; t := x
; x := y
; y := t
	{x = Y and y = X}

