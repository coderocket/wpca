module AST where

data Node = Spec Node Node Node Node | Locals [ Node ] | Declaration [ String ] Node | Assign ([String],[Node]) | Loop [(Node, Node)] | Cond [(Node,Node)] | Seq Node Node | Skip | Plus Node Node | Minus Node Node | Times Node Node | Var String | TypeVar String | Nat Int | Neg Node | Quotient Node Node | Div Node Node | Mod Node Node  | NodeEq Node Node | NodeGeq Node Node | NodeLeq Node Node | Conj Node Node | Disj Node Node | Implies Node Node | PredTrue | PredFalse

declsToList ds = foldr (++) [] (map f ds) 
  where f (Declaration ds t) = foldr (\ x xs -> (x,t):xs) [] ds

