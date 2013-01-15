
proc Partition[i : out int, a : out array of N int , N : nat, b,e:int]
	{ a = A and N >= e-b and e-b > 1 }
; skip
	{ b < i < e and nochange[0,b,a,A] and nochange[e,N,a,A] and permutation[b,e,a,A] and (all j : b..i-1 | a[j] <= A[0]) and all j : i..e-1 | a[j] >= A[0] } 

proc Sort[f : out array of M int, M : nat, x,y : int] 
	{ f = F and 0 <= x <= y and y <= M }
  k : int
; if y - x <= 1 -> skip
  [] y - x  > 1 -> Partition[k,f,M,x,y] ; Sort[f,M,x,k] ; Sort[f,M,k,y]
  fi
	{ permutation[x,y,f,F] and nochange[0,x,f,F] and nochange[y,M,f,F] and all u,v : x .. y-1 | u <= v => f[u] <= f[v] }
