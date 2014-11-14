
-- Define our monad instance
instance Functor     Cu where
  map f (CuState (Err str)) = CuState $ Err str
  map f (CuState x)         = CuState (map f x)

instance Applicative Cu where
  (CuError l1) <$> (CuError l2) = CuError (l1 ++ l2)
  (CuError l1) <$> _            = CuError (l1)
  _            <$> (CuError l2) = CuError (l2)
  (CuState x)  <$> (CuState y)  = CuState (x <$> y)
  pure r = CuState $ ST (\d => (d, r))

lift : Cu a -> (a -> Cu b) -> Cu b
lift (CuError l) _ = CuError (l)
lift (CuState (ST ops)) f = h
    where 
      h : (Context, a) -> Cu b
      h (context, aVal) with (f aVal) 
        | (CuError l) = (CuError l) 
        | (CuState succ) = CuState $ ST $ (\_ => succ context) 

instance Monad       Cu where
  (>>=) = lift
{-    where 
      f' : a -> Cu b
      f' aVal = CuState (f a)
      -}

