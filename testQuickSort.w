
proc Partition[i : out int; a : array of N int ; N : nat; b,e:int]
	{ a = A and N >= e-b and e-b > 1 }
; skip
	{ b < i < e and permutation[a,A] and (all j : b..i-1 | a[j] <= A[0]) and all j : e..N-1 | a[j] >= A[0] } 

proc Sort[f : out array of M int; M : nat; x,y : int] 
  k : int
	{ f = F }
; if y - x <= 1 -> skip
  [] y - x  > 1 -> Partition[k,f,M,x,y] ; Sort[f,M,x,k] ; Sort[f,M,k,y]
  fi
	{ all u,v : x .. y-1 | u <= v => f[u] <= f[v] }
