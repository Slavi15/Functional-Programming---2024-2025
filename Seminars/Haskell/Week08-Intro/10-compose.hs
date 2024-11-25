compose :: (b -> c) -> (a -> b) -> a -> c
compose f g x = f $ g $ x

(*) :: (b -> c) -> (a -> b) -> a -> c
(*) f g x = f $ g $ x