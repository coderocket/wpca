
proc A[x : int, y : out int]
	{ x > 0 }
; y := x - 1
	{ y >= 0 }
	
proc B[x : out int]
	{ x > 0 }
  y : int
; A[x,y]; A[y, x]
	{ y >= 0 }
