
proc P[x,y : int]
	{ x < y }
; y := x
; keeping
    x < y
  do x = y -> x := x - 1
  od
	{ x > y }
