-- Prolog IO

{--
FREE MONADS
In general, a structure is called free when it is left-adjoint to a forgetful functor.
In this specific instance, the Term data type is a higher-order functor that maps
a functor f to the monad Term f ; this is illustrated by the above two instance
definitions. This Term functor is left-adjoint to the forgetful functor from monads
to their underlying functors.
--}

data Term f a = Pure a 
			  | Impure (f (Term f a))

main 							= undefined

instance Functor f => Functor (Term f) where
	fmap f (Pure x ) 			= Pure (f x )
	fmap f (Impure t) 			= Impure (fmap (fmap f ) t)

instance Functor f => Monad (Term f) where
	return x 					= Pure x
	(Pure x ) 	>>= 	f 		= f x
	(Impure t) 	>>= 	f 		= Impure (fmap (>>= f ) t)
