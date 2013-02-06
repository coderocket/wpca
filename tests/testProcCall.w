
proc F[x : out int] 
	{ x = X }
; x := x + 1
	{ x = X+1 }
	
proc G[x: out  int]
	{ x = X }
; F[x]; { x = X + 1 } ; F[x]
	{ x = X + 2 }
