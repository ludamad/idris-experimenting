module Main

data Box a b = BSucc (a -> Pair a b) 

comp : Box a b -> (Pair a b -> Pair a c) -> Box a c
comp (BSucc f) pf = BSucc (pf . f)

fcomp : Box a (b -> c) -> Pair a b -> Pair a c
fcomp (BSucc f) (state1, bVal) with (f state1)  
  | (state2, bFunc) = (state2, bFunc bVal)

instance Functor (Box dT) where
  map f (BSucc succ) = BSucc next
    where
      next prev with (succ prev) |(state, x) = (state, f x)

instance Applicative (Box dT) where
  x <$> y = comp y (fcomp x)
  pure r = BSucc (\d => (d, r))

lift : Box dT x -> (x -> Box dT y) -> Box dT y
lift (BSucc succ) f = BSucc box
  where box s1 with (succ s1)
          |(s2, xVal) with (f xVal)
             | BSucc succ' = (succ' s2)

instance Monad (Box dT) where
  x >>= f = lift x f

get : Box a a
get = BSucc (\state => (state, state))

put : a -> Box a ()
put state = BSucc (\_ => (state, ()))

boxDo : a -> Box a b -> Pair a b
boxDo state (BSucc f) = f state 

MList : Type -> Type
MList a = Box (List String) a

addStr : String -> MList ()
addStr s = do put (s :: !get)

addStrs : List String -> MList ()
addStrs []     = return ()
addStrs (h::t) = do {addStr h ; addStrs t}

main : IO()
main = do
  putStrLn "Hellope"
  let (state, _) = boxDo [] (addStr "hullo")
  putStrLn $ show stat
--  (state, _) <- return $ boxDo [] ( )
    
