
proc Partition[ a : array of N int ; N : nat; i : out int]
	{ true }
; skip
	{ true }

proc Sort[f : out array of M int; M : nat] 
  k : int
	{ f = F }
; Partition[f,M,k] ; Sort[f,M] 
	{ true }
