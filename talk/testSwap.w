proc Swap[x, y : out int]
	{x = X and y = Y}
  t : int
; t := x
; y := t
; x := y
	{x = Y and y = X}

