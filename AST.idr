module AST

import Data.SortedMap

import Types

data Op = Plus | Minus | Mult | Divide

-- Commands and expressions

-------------------------------------------------------------------------------
-- Store information needed for, eg, error messages.
-------------------------------------------------------------------------------
record ModuleMetadata : Type where
  MkModuleMetadata : 
    (filename : String) ->
    ModuleMetadata

record NodeMetadata : Type where
  MkNodeMetadata : 
    (moduleMetadata : ModuleMetadata) ->
    (posNum : Int) ->
    (lineNum : Int) ->
    NodeMetadata

-------------------------------------------------------------------------------
-- Store optionally typed expression nodes
-------------------------------------------------------------------------------
data Expr 
record ExprNode : Type where
    MkExprNode :
       (nodeMetadata : NodeMetadata) ->
       (typeRef : Int) -> -- Index to TypeRefs
       (expr : Expr) ->
       ExprNode

data Expr    = VarNode String
             | IntNode Int
             | FloatNode Float
             | StringNode String
             | ListNode (List ExprNode)
             | OpNode Op ExprNode ExprNode
             | CallNode ExprNode (List ExprNode)

-------------------------------------------------------------------------------
-- Store information needed for, eg, error messages.
-------------------------------------------------------------------------------
data Command 
record CommandNode : Type where
    MkCommandNode :
       (nodeMetadata : NodeMetadata) ->
       (command : Command) ->
       CommandNode


record Assign : Type where
  MkAssign :
    (modifier : TypeModifier) ->
    (varName : String) ->
    (varName : String) ->
    Assign

data Command = CAssign String ExprNode 
             | CSeq CommandNode CommandNode
             | WhileLoop CommandNode CommandNode
             | ForLoop ExprNode CommandNode

Vars : Type
Vars = SortedMap String ExprNode

emptyVars : Vars
emptyVars = fromList []

