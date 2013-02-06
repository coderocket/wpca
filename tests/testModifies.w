
record A {
  x : int
}

proc M[a : A] modifies x
	{ true }
; skip
	{ a.x = 1 }
	
proc G[a : A] modifies x
	{ true }
; M[a]
	{ a.x < 3 }
