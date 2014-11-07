module TypeCheck

import AST
import Data.SortedMap
import Types


total
lookupS : String -> TypeRefs -> TypeRef -> TypeRefs
lookupS str trefs with (getS trefs str)

-- One pass for expressions
total
unifyTypes : (TypeRef, TypeRef) -> Maybe (TypeRef, TypeRef)
unifyTypes (node, tref, trefs) with (node)
    | (MkExprNode mdata eType expr) = uni expr
    where 
      uni : Expr -> Maybe (ExprNode, TypeRef, TypeRefs)
      uni (VarNode    str     ) = Just (node, tref, trefs)
      uni (IntNode    int     ) = Just (node, tref, trefs)
      uni (FloatNode  float   ) = Just (node, tref, trefs)
      uni (StringNode str     ) = Just (node, tref, trefs)
      uni (ListNode   elist   ) = Just (node, tref, trefs)
      uni (OpNode     op e1 e2) = Just (node, tref, trefs)
      uni (CallNode   e1 eargs) = Just (node, tref, trefs)

-- One pass for expressions
total
unifyExpr : (ExprNode, TypeRef, TypeRefs) -> Maybe (ExprNode, TypeRef, TypeRefs)
unifyExpr (node, tref, trefs) with (node)
    | (MkExprNode mdata eType expr) = uni expr
    where 
      uni : Expr -> Maybe (ExprNode, TypeRef, TypeRefs)
      uni (VarNode    str     ) = Just (node, tref, trefs)
      uni (IntNode    int     ) = Just (node, tref, trefs)
      uni (FloatNode  float   ) = Just (node, tref, trefs)
      uni (StringNode str     ) = Just (node, tref, trefs)
      uni (ListNode   elist   ) = Just (node, tref, trefs)
      uni (OpNode     op e1 e2) = Just (node, tref, trefs)
      uni (CallNode   e1 eargs) = Just (node, tref, trefs)

main : IO ()
main = putStrLn "Hello World"
