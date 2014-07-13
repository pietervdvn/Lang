It might seem strange, but when a function is applied, it's types are bound.

Take '² : Nat -> Nat'. When it is applied to '5:Nat', the type is '(Nat -> Nat) Nat', or simply 'Nat'. We reduced the *curry* in the type, by binding the first type in the curry with 'Nat'. As both were the same, we simply dropped it.

Take 'id : a -> a', applied to '5 : Nat'. The type of 'id 5' is '(a -> a) Nat'. To apply nat to the first argument of the curry, we have to bind 'a' to 'Nat'. We do this by replacing all other occurences of 'a' with 'Nat': '(Nat -> Nat) Nat' and applying as before: 'Nat'.

When we can not bind, e.g. '(Int -> Int) Nat', it means a type error happened.

This must also happen recursively, e.g. '(m a -> (a -> m b) -> m b) (State Int Bool)'. 
Here, 'm a' must be bound to 'State Int Bool'. When the bindings are derived, we find that 'm' must be bound to 'State Int' and 'a' must be bound to 'Int': '(State Int Bool -> (Bool -> State Int b) -> State Int b) (State Int Bool)'. We can then apply without problem: '(Bool -> State Int b) -> State Int b)


(a -> b -> c) (d -> e) => (a == d; (b -> c) == e)
