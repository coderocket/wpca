proc Partition[N : nat ; a : array of N int ; pivot : int; i : nat ; l,u : int]
	{ a = A and N > 1 and A[0] = pivot }
; l,u := 1,N-1
; if a[N-1] < pivot -> a[0],a[N-1] := a[N-1],a[0]
  [] a[N-1] >= pivot -> skip
  fi
; keeping
	0 < l < N and 0 < u < N and l <= u and permutation[a,A] and (all j : 0..l-1 | a[j] <= pivot) and all j : u..N-1 | a[j] >= pivot  
  do l != u -> 
    if a[l] <= pivot -> l := l + 1
    [] a[u-1] >= pivot -> u := u - 1
    [] a[l] > pivot and a[u-1] < pivot -> a[l],a[u-1] := a[u-1],a[l]
    fi
  od
; i := l
	{ 0 < i < N and permutation[a,A] and (all j : 0..i-1 | a[j] <= pivot) and all j : i..N-1 | a[j] >= pivot } 
