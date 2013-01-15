
proc P[x,y : int]
	{ x < y }
; if x < y  -> skip
  [] x >= y -> skip
  fi
; if x < y -> x := x + 1
  [] x >= y -> skip
  fi
	{ x >= y }
