  head : Node
; newHead : Node
	{ heap = H and seq[head, S] }
; skip
	{ seq[newHead, S] }
	{ no newHead.*next & H }

where

pred seq[x : Node, s : seq T] {
 all i : 0..#s-1 | at[x,i, s[i]]
 len[x,#s]
}

