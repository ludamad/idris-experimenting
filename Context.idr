module Context

import Data.SortedMap
import Control.Monad.State

-------------------------------------------------------------------------------
-- Types for the context object & accompanying convenience monad
-------------------------------------------------------------------------------

data AType : Type -- Resolved type

data AId = ARef Int | AVar String
instance Eq AId where
  (ARef _) == (AVar _) = False
  (AVar _) == (ARef _) = False
  (ARef x) == (ARef y) = (x == y)
  (AVar x) == (AVar y) = (x == y)

-- Type formula has list of id subtypes, type subtypes, and type supertypes:
data ATypeBox =  F (Maybe AType) ((List AId), (List AType), (List AType)

data TypeMap = MkStore Int (SortedMap Int ATypeBox)

emptyStore : TypeMap
emptyStore = MkStore 1 (SortedMap.fromList [])

insert : Int -> ATypeBox -> TypeMap -> TypeMap
insert id tref (MkStore nextCnt map) = MkStore nextCnt $ insert id tref map

lookup : Int -> TypeMap -> Maybe ATypeBox
lookup id (MkStore nextCnt map) = lookup id map

alloc : TypeMap -> (Int, TypeMap)
alloc (MkStore nextCnt map) = (nextCnt+1, MkStore (nextCnt+1) map)

-- Convenient type aliases:
Vars : Type
Vars = SortedMap String ATypeBox 

data ContextMetadata = CmNone

-- The context object:
record Context : Type where
  MkContext : 
   (contextMetadata : ContextMetadata) ->
   (contextMetadata : ContextMetadata) ->
   (vars : Vars) ->
   -- Holds intermediate results for expression types
   (typeMap : TypeMap) ->
   Context

-- The state monad for context:
C : Type -> Type
C a = StateT Context Maybe a

------------------------------------------------------------------------------
-- Types for Apollo types, possibly before inference.
-------------------------------------------------------------------------------

data APrim = AInt | AString
instance Eq APrim where
  AInt == AString = False
  AString == AInt = False
  x == y          = True

data AType = MkPrim APrim
instance Eq AType where
  (MkPrim p1) == (MkPrim p2) = p1 == p2

refGetC : AId -> Context -> Maybe ATypeBox
refGetC (ARef i) con  = lookup i (typeMap con)
refGetC (AVar i) con = lookup i (vars con)

refGet : AId -> C (Maybe ATypeBox)
refGet id = do return $ refGetC id !get

refPutC : AId -> ATypeBox -> Context -> Context
refPutC (ARef i) t con  = set_typeMap (insert i t (typeMap con)) con
refPutC (AVar i) t con = set_vars    (insert i t (vars con))    con

refPut : AId -> ATypeBox -> C ()
refPut id t = do modify $ refPutC id t

allocT : C ATypeRef
allocT = do 
  let (id, map') = alloc (typeMap !get)
  modify $ set_typeMap map'
  return $ Right $ ARef id

unifyT : AType -> AType -> C ()
unifyT t1 t2 =  if t1 /= t2 then ST (\_ => Nothing) 
                            else pure ()

-- Type formula manipulation:

addId : (List AId, List AType) -> AId -> (List AId, List AType)
addId (ids, types) id = (
    if id `elem` ids 
      then (ids, types) 
      else (id :: ids, types))

addType : (List AId, List AType) -> AType -> (List AId, List AType)
addType (ids, types) type = (
    if type `elem` types
      then (ids, types) 
      else (ids, type :: types))

unifyII : AId -> AId -> C ()
unifyII id1 id2 = do
    let c = !get 
    u (refGetC id1 c) (refGetC id2 c)
 where
   -- Unify Form and Nothing
   uFN : AId -> AId -> ATypeBox -> C ()
   uFN idA idB (Left type)  = refPut idB $ Right ([], [type])
   uFN idA idB (Right form) = refPut idA $ Right $ addId form idB

   -- Unify Form and Form
   uFF : AId -> AId -> ATypeBox -> ATypeBox -> C ()
   -- We are saing type > form, add to bigger-than list (of types)
   uFF idA idB (Left type)  (Right form) = refPut idB $ Right $ addType form type 
   -- We are saing form > type, add to bigger-than list (of types)
   uFF idA idB (Right form) (Left type)  = refPut idB $ Right $ addType form type 
   uFF idA idB (Right form) (Right form) = refPut idA $ Right $ addId form idB

   -- Unify Maybe and Maybe
   u : Maybe ATypeBox -> Maybe ATypeBox -> C ()
   u Nothing _ = refPut id1 $ Right ([id2],[],[])
   u (Just form) Nothing = uFN id1 id2 form 
   u (Just form1) (Just form2) = uFF id1 id2 form1 form2
     refPut id1 $ unifyFI form id2

-------------------------------------------------------------------------------
-- AST node definitions
-------------------------------------------------------------------------------

data AOp = Plus | Minus | Mult | Divide

-- Store information needed for, eg, error messages.
record ModuleMetadata : Type where
  MkModuleMetadata : 
    (filename : String) ->
    ModuleMetadata

-- Store information needed for, eg, error messages.
record NodeMetadata : Type where
  MkNodeMetadata : 
    (moduleMetadata : ModuleMetadata) ->
    (posNum : Int) ->
    (lineNum : Int) ->
    NodeMetadata

-- Store optionally typed expression nodes
data Expr 
record ExprNode : Type where
    MkExprNode :
       (nodeMetadata : NodeMetadata) ->
       (type : ATypeRef) ->
       (expr : Expr) ->
       ExprNode

data Literal = LitString String | LitInt Int | LitFloat Float

data Expr    = VarNode String
             | LitNode Literal
             | ListNode (List ExprNode)
             | OpNode AOp ExprNode ExprNode
             | CallNode ExprNode (List ExprNode)

-- An imperative command
data Command 
record CommandNode : Type where
    MkCommandNode :
       (nodeMetadata : NodeMetadata) ->
       (command : Command) ->
       CommandNode

data Command = CLet ATypeRef String ExprNode
             | CAssign String ExprNode 
             | CSeq CommandNode CommandNode
             | WhileLoop CommandNode CommandNode
             | ForLoop ExprNode CommandNode

-- 





