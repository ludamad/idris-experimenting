
module Main 
x : Nat 
x = 0 

y : Nat 
y = 1 

z : Nat 
z = 2

data ExprNode = 
  Constant Float |
  AddNode ExprNode ExprNode |
  MultNode ExprNode ExprNode


U : (Nat -> Float) -> Nat -> Float -> (Nat -> Float)
U f id v idx = if idx == id then v else f idx

expr_ex4 : ExprNode
expr_ex4 = AddNode (Constant 2.0) (Constant 2.0)

expr_exn : Float -> ExprNode
expr_exn n = AddNode (Constant n) (Constant n)

add_0 : (a:Nat) -> (a + Z = a)
add_0 Z = Refl
add_0 (S n) = ?ppp

add_comm : (a:Nat) -> (b:Nat) -> (a + b = b + a)
add_comm Z b = ?proof1
add_comm a b = ?proof2

main : IO ()
main = do putStr "Result is: OK"
--          putStrLn (show (eval expr_ex4))

