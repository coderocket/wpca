module AST where

data Node = Spec Node Node Node Node | Locals [ Node ] | Declaration [ String ] Node | Assign ([String],[Node]) | Loop Node [(Node, Node)] | Cond [(Node,Node)] | Seq Node Node | Skip | Var String | TypeVar String | Nat Int | Neg Node | PredTrue | PredFalse | Const Node | BinOp BinOpKind Node Node | Not Node

data BinOpKind = Plus| Minus | Times | Quotient | Div | Mod  | NodeEq | NodeGeq | NodeLeq | Conj | Disj | Implies | Join | NodeGreater | NodeLess  

declsToList ds = foldr (++) [] (map f ds) 
  where f (Declaration ds t) = foldr (\ x xs -> (x,t):xs) [] ds

