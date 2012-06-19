  N : nat
; a : array of N Comparable
; i : nat
; l,u : int
	{ a = A and N > 1 and A[0] = pivot }
; l,u := 1,N-1
; if lt[a[N-1],pivot] -> a[0],a[N-1] := a[N-1],a[0]
  [] gte[a[N-1],pivot] -> skip
  fi
; keeping
	0 < l < N and 0 < u < N and l <= u and permutation[a,A] and (all j : 0..l-1 | lte[a[j],pivot]) and all j : u..N-1 | gte[a[j],pivot]
  do l != u -> 
    if lte[a[l],pivot] -> l := l + 1
    [] gte[a[u-1],pivot] -> u := u - 1
    [] gt[a[l],pivot] and lt[a[u-1],pivot] -> a[l],a[u-1] := a[u-1],a[l]
    fi
  od
; i := l
	{ 0 < i < N and permutation[a,A] and (all j : 0..i-1 | lte[a[j],pivot]) and all j : i..N-1 | gte[a[j],pivot] } 
