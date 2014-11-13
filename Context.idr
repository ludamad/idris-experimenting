module Context

import SortedMap

data AType : Type -- Apollo type

Vars = Type
Vars = SortedMap Int AType

data ContextMetadata = CmNone

-- The context monad:
record C : (retType : Type) -> Type where
  MkC : () ->
   (contextMetadata : ContextMetadata) ->
   (vars : Vars) ->
   (typeRef : Int) -> -- Index to TypeRefs

   (expr : Expr) ->
   (r : retType) ->
   C r

data ATypeRef = 
