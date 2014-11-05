import Data.Hash

data Op = Plus | Minus | Mult | Divide

data ExprNode = VarNode String
              | IntNode Int
              | FloatNode Float
              | StringNode String
              | ListNode (List ExprNode)
              | CallNode ExprNode ExprNode -- Represent call chains 'currying' style
              | OpNode Op ExprNode ExprNode

data Evaluated : ExprNode -> Type where

data NoVar : ExprNode -> Type where
  NVIntNode : (e : ExprNode) -> 
             (i:Int) -> (e = IntNode i) ->
             NoVar e
  NVFloatNode : (e : ExprNode) -> 
             (i:Float) -> (e = FloatNode i) ->
             NoVar e
  NVStringNode : (e : ExprNode) -> 
             (i:String) -> (e = StringNode i) ->
             NoVar e
  NVNilNode : (e : ExprNode) -> 
             (e = ListNode []) ->
             NoVar e
  NVListNode : (e : ExprNode) -> (head : ExprNode) -> (rest : List ExprNode) -> 
             (e = ListNode (head::rest)) -> 
             NoVar head -> NoVar (ListNode rest) ->
             NoVar e
  NVOpNode : (e,e1,e2 : ExprNode) -> (op : Op) -> (e = OpNode op e1 e2) ->
             NoVar e1 -> NoVar e2 ->
             NoVar e
  NVCallNode : (e,e1,e2 : ExprNode) -> (e = CallNode e1 e2) ->
             NoVar e1 -> NoVar e2 ->
             NoVar e


-- Example, 1+1 has no variable lookups
thm_novar_ex1 : NoVar (OpNode Plus (IntNode 1) (IntNode 1))
thm_novar_ex1 = ?prf_novar_ex1

data VNode : Type where
  MkVNode : (expr : ExprNode) -> (nv : NoVar expr) -> VNode

Vars : Type
Vars = String -> VNode

eval : Vars -> ExprNode -> VNode
eval vars e_ with (e_)
  | (VarNode str) = vars str
  | (IntNode f) = let e=(IntNode f) in 
     MkVNode e (NVIntNode e f Refl)
  | (StringNode f) = let e=(StringNode f) in 
     MkVNode e (NVStringNode e f Refl)
  | (FloatNode f) = let e=(FloatNode f) in 
     MkVNode e (NVFloatNode e f Refl)
  | (ListNode []) = let e = (ListNode []) in MkVNode e (NVNilNode e Refl)
  | (CallNode e1 e2) with (eval vars e1)
     | (MkVNode e1' nv1') with (eval vars e2) 
        | (MkVNode e2' nv2') = MkVNode 
            (CallNode e1' e2')  
            (NVCallNode (CallNode e1' e2') e1' e2' Refl nv1' nv2')
  | (OpNode op e1 e2) with (eval vars e1)
     | (MkVNode e1' nv1') with (eval vars e2) 
        | (MkVNode e2' nv2') = MkVNode 
            (OpNode op e1' e2')  
            (NVOpNode (OpNode op e1' e2') e1' e2' op Refl nv1' nv2')

main : IO ()
main = putStrLn "Hello World!" 

---------- Proofs ----------

Main.prf_novar_ex1 = proof
  refine NVOpNode
  exact (OpNode Plus (IntNode 1) (IntNode 1))
  exact (IntNode 1) ; exact (IntNode 1)
  exact Plus
  compute
  trivial
  exact (NVIntNode (IntNode 1) 1 Refl)
  exact (NVIntNode (IntNode 1) 1 Refl)


