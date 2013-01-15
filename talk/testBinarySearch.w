proc binarySearch[x : int, N : nat, a : array of N int, p : out int]
  { all i,j : 0..N-1 | i <= j => a[i] <= a[j] }
  b,e,m : int
; b,e := 0,N
; keeping
   0 <= b <= e and e <= N and (all i : 0..b-1 | a[i] <= x) and (all i : e..N-1 | a[i] >= x) 
  do b != e -> 
        m := (b+e) div 2 
      ; if a[m] >= x -> e := m 
        [] a[m] < x -> b := m + 1 
        fi
  od
; p := b
  { (all i : 0..p-1 | a[i] <= x) and (all i : p..N-1 | a[i] >= x) }
	
