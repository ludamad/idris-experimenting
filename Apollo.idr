import Data.SortedMap

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
-- The various 
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Store optionally typed expression nodes
-------------------------------------------------------------------------------
data Expr 
record ExprNode : Type where
    MkExprNode :
       (nodeMetadata : NodeMetadata) ->
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

data VarModifier = VMutable | VValue

record Assign : Type where
  (modifiers : [VarModifier] ->
  (varName : String) ->
  (varName : String) ->
  Assign

data Command = CAssign String ExprNode 
             | CSeq CommandNode CommandNode
             | WhileLoop CommandNode CommandNode
             | ForLoop ExprNode CommandNode

Vars : Type
Vars = SortedMap String (Maybe ExprNode)

-- Expression evaluation
{-
doOp : Op -> Maybe ExprNode -> Maybe ExprNode -> Maybe ExprNode
doOp Plus (Just (IntNode i)) (Just (FloatNode f)) = Just (FloatNode ((prim__toFloatInt i)+f))
doOp Plus (Just (IntNode i)) (Just (IntNode f)) = Just (IntNode (i+f))
doOp Plus (Just (FloatNode i)) (Just (FloatNode f)) = Just (FloatNode (i+f))
doOp op e1 e2 = Nothing -- Catchall

total
expr_eval : Vars -> ExprNode -> Maybe ExprNode
expr_eval vars (VarNode str) = vars str
expr_eval vars (IntNode f) = Just (IntNode f) 
expr_eval vars (StringNode f) = Just (StringNode f) 
expr_eval vars (FloatNode f) = Just (FloatNode f) 
expr_eval vars (ListNode l) = assert_total $ do 
   l' <- sequence (map (expr_eval vars) l)
   return (ListNode l')
expr_eval vars (OpNode op e1 e2) = doOp op (expr_eval vars e1) (expr_eval vars e2)

total
show_expr : ExprNode -> String
show_expr (VarNode str) = "*" ++ str
show_expr (IntNode f) = show f
show_expr (StringNode f) = f
show_expr (FloatNode f) = show f
show_expr (ListNode l) = assert_total $ show (map show_expr l)
show_expr (OpNode op e1 e2) =  (show_expr e1) ++ "<*>" ++ (show_expr e2)

total show_exprM : Maybe ExprNode -> String
show_exprM (Just e) = show_expr e
show_exprM Nothing = "<nil>"

main : IO ()
main = putStrLn "Hello World!" 

-- Program evaluation

total
update : Vars -> String -> Maybe ExprNode -> Vars
update vars key val str = if str == key then val else vars str

total
eval : Vars -> Command -> Vars
eval vars (CAssign str expr) = update vars str (expr_eval vars expr)
eval vars (CSeq c1 c2) = eval (eval vars c1) c2

-- "Tests":

sample_state : String -> Maybe ExprNode
sample_state str = case str of
    "X" => Just (IntNode 1)
    "Y" => Just (FloatNode 1.0)
    "Z" => Just (StringNode "Hello World")
    s => Nothing

sample_expr : ExprNode
sample_expr = OpNode Plus (VarNode "X") (VarNode "X")

sample_expr_test : (expr_eval sample_state sample_expr = Just (IntNode (2)))
sample_expr_test = proof {trivial}

sample_prog : Command
sample_prog = CAssign "X" sample_expr

sample_prog_test : (eval sample_state sample_prog) "X" = (Just (IntNode 2))
sample_prog_test = proof {trivial}
-}
