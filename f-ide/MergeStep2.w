
proc Merge[xs : array of N int, N : nat, ys : array of M int, M : nat, result : array of K int, K : nat]
	{ K = N + M and sorted[xs] and sorted[ys] }
; skip
	{ sorted[prefix[result,K]] and permutation[prefix[result,K], append[prefix[xs,N], prefix[ys,M]]] }

theory arrays
