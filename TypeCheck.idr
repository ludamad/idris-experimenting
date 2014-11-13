module TypeCheck

import AST
import Data.SortedMap
import Types

-- Given an operator that does mutations, takes care of recreating the tree structure
-- and calling it recursively on each node.

total 
%assert_total
assignTreeIndices : Int -> ExprNode -> ExprNode
assignTreeIndices idx node = record { expr = (up (expr node)), typeRef = idx} node
    where 
      listup : Int -> List ExprNode -> List ExprNode
      listup i [] = []
      listup i (h :: t) = (assignTreeIndices i h) :: (listup (i+1) t)

      up : Expr -> Expr
      up (VarNode    str     ) = expr node
      up (LitNode    lit     ) = expr node
      up (ListNode   elist   ) = ListNode (listup (idx+1) elist)
      up (OpNode     op e1 e2) = OpNode op (assignTreeIndices (idx+1) e1) (assignTreeIndices (idx+2) e2)
      up (CallNode   e1 eargs) = CallNode (assignTreeIndices (idx+1) e1) (listup (idx+2) eargs)

total
lookupS : String -> TypeRefs -> TypeRef -> TypeRefs
--lookupS str trefs with (getS trefs str)

-- One pass for expressions
{-total
unifyTypes : (TypeRef, TypeRef) -> Maybe (TypeRef, TypeRef)
unifyTypes (node, tref, trefs) with (node)
    | (MkExprNode mdata eType expr) = uni expr
    where 
      uni : Expr -> Maybe (ExprNode, TypeRef, TypeRefs)
      uni (VarNode    str     ) = Just (node, tref, trefs)
      uni (LitNode    lit     ) = Just (node, tref, trefs)
      uni (ListNode   elist   ) = Just (node, tref, trefs)
      uni (OpNode     op e1 e2) = Just (node, tref, trefs)
      uni (CallNode   e1 eargs) = Just (node, tref, trefs)
      -}
-- One pass for expressions
total
unifyExpr : (ExprNode, TypeRef, TypeRefs) -> Maybe (ExprNode, TypeRef, TypeRefs)
unifyExpr (node, tref, trefs) with (node)
    | (MkExprNode mdata eType expr) = uni expr
    where 
      uni : Expr -> Maybe (ExprNode, TypeRef, TypeRefs)
      uni (VarNode    str     ) = Just (node, tref, trefs)
      uni (LitNode    lit     ) = Just (node, tref, trefs)
      uni (ListNode   elist   ) = Just (node, tref, trefs)
      uni (OpNode     op e1 e2) = Just (node, tref, trefs)
      uni (CallNode   e1 eargs) = Just (node, tref, trefs)

main : IO ()
main = putStrLn "Hello World"
