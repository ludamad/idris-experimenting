module State

-- While in the standard library, I redid this for practice with monads.
-- Encapsulates 'mutable' state by allowing operations to be strung
-- behind the scenes.
data State : -> a -> b -> Monad m ->  Type where
  ST : (a -> (a, m b)) -> State m a b

{-
-- Helper functions
comp : State a b -> ((a, b) -> (a, c)) -> State a c
comp (ST f) pf = ST (pf . f)

fcomp : State a (b -> c) -> (a, b) -> (a, c)
fcomp (ST f) (state1, bVal) with (f state1)  
  | (state2, bFunc) = (state2, bFunc bVal)

lift : State dT x -> (x -> State dT y) -> State dT y
lift (ST succ) f = ST box
  where box s1 with (succ s1)
          |(s2, xVal) with (f xVal)
             | ST succ' = (succ' s2)
get : State a a
get = ST (\state => (state, state))

put : a -> State a ()
put state = ST (\_ => (state, ()))

eval : a -> State a b -> (a, b)
eval state (ST f) = f state 

-- Define our monad instance
instance Functor (State dT) where
  map f (ST succ) = ST next
    where
      next prev with (succ prev) |(state, x) = (state, f x)

instance Applicative (State dT) where
  x <$> y = comp y (fcomp x)
  pure r = ST (\d => (d, r))

instance Monad (State dT) where
  (>>=) = lift-}
