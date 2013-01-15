proc Partition[N : nat ; a : array of N Comparable ; k,i : nat]
	{ a = A and N > 1 and a[0] = pivot }
; if lt[a[1],a[0]] -> a[0],a[1] := a[1],a[0] 
  [] gte[a[1],a[0]] -> skip
  fi
; i,k := 1,2
; keeping
	0 < i < k and 1 < k <= N and permutation[a,A] and (all j : 0..i-1 | lte[a[j], pivot]) and all j : i..k-1 | gte[a[j], pivot] 
  do k != N -> 
    if gte[a[k], pivot] -> k := k + 1
    [] lte[a[k], pivot] -> a[i],a[k] := a[k],a[i] ; i,k := i+1,k+1
    fi
  od
	{ 0 < i < N and permutation[a,A] and (all j : 0..i-1 | lte[a[j], pivot]) and all j : i..N-1 | gte[a[j], pivot] } 
