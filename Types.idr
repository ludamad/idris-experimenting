module Types

import Data.SortedMap
{-
record TypeFormStore : Type where
  MkTypeFormStore :
    (var2id : (SortedMap String Int)) ->
    (forms : (SortedMap Int TypeForm)) ->
    TypeFormStore

-- The type checker monad:
data CheckState : (a : Type) -> Type where
  MkState : (x : a) -> CheckState a
  ErrorState : (errorMessage : String) -> CheckState a
  -}
{-
instance Applicative CheckState where
  (ErrorState msg) >>= _ = (ErrorState msg)
instance Monad CheckState where

  (ErrorState msg) >>= _ = (ErrorState msg)-}


-- Note: TrRef uses an index to link equivalent but still uninferred types
data TypeRef = TrList (List TypeRef) | TrPrim String | TrRef Int

-- Note: TmRef uses an index to link equivalent but still uninferred types
data TypeModifier = TmMissing | TmMutable | TmValue | TmRef Int 

TypeRefs : Type
TypeRefs = (SortedMap Int TypeRef, SortedMap String TypeRef)

emptyTypeRefs : TypeRefs
emptyTypeRefs = (fromList [], fromList [])

-- I : Integer
-- S : String

setI : TypeRefs -> Int ->  TypeRef -> TypeRefs
setI (imap, smap) i tr = (insert i tr imap, smap)

getI : TypeRefs -> Int ->  Maybe TypeRef 
getI (imap, smap) i = SortedMap.lookup i imap

setS : TypeRefs -> String ->  TypeRef -> TypeRefs
setS (imap, smap) s tr = (imap, insert s tr smap)

getS : TypeRefs -> String ->  Maybe TypeRef 
getS (imap, smap) s = SortedMap.lookup s smap


