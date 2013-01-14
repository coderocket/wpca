proc Max[x, y : int, m : out int]
	{ true }
; if x < y -> m := y
  [] x > y -> m := y
  [] x = y -> skip
  fi
	{ m = max[x,y] }

