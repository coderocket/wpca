
proc Merge[xs : array of N int, N : nat, ys : array of M int, M : nat, result : array of K int, K : nat]
	{ K = N + M and sorted[xs] and sorted[ys] }
  n,m,k : nat
; n,m,k := 0,0,0
; keeping
	k = n + m and sorted[prefix[result,k]] and permutation[prefix[result,k], append[prefix[xs,n], prefix[ys,m]]] 
  do n != N and m != M ->	
		if xs[n] < ys[m] -> result[k],k,n := xs[n], k+1, n+1
		[] xs[n] >= ys[m] -> result[k],k,m := ys[m], k+1, m+1 
    fi
  [] n = N -> skip
  [] m = M -> skip
  od 
	{ sorted[prefix[result,K]] and permutation[prefix[result,K], append[prefix[xs,N], prefix[ys,M]]] }

theory arrays
