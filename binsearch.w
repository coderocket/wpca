  x : int 
; N : nat
; f : array of N int
; p,q,m : int
  { all i,j : 0..N-1 | i <= j => f[i] <= f[j] }
; p,q := -1,N
; keeping 
   -1<= p and p < q and q <= N and f[p] <= x and x < f[q] 
  do p + 1 != q ->
	m := (p+q) div 2 
	; if f[m] <= x -> p := m
          [] f[m] > x -> q := m
	  fi
  od
; if p != -1 and f[p] != x -> p := -1 
  [] p = -1 or f[p] = x -> skip
  fi
  { (p = -1 and no i:0..N-1 | f[i] = x) or (0 <= p and p < N and f[p] = x) }
	
