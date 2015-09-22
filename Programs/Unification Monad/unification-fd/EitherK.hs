

-- Mehul Solanki.

-- EitherK from unification-fd.


import Control.Monad.EitherK

{--
1. Short Circuiting Monad

1.1 runEitherK :: EitherK e a -> Either e a
runEitherK (function (params))


1.2 toEitherK :: Either e a -> EitherK e a
runEitherK (toEitherK (Right 0))

1.3 eitherK :: (e -> b) -> (a -> b) -> EitherK e a -> b
eitherK (+1) (+2) (toEitherK (Left 0))
1
eitherK (+1) (+2) (toEitherK (Right 0))
2

1.4 throwEitherK :: e -> EitherK e a
runEitherK (throwEitherK (error "hello"))
Left *** Exception: hello

1.5 catchEitherK :: EitherK e a -> (e -> EitherK f a) -> EitherK f a

runEitherK $ catchEitherK (toEitherK (Left 1)) (throwEitherK) 
Left 1

runEitherK $ catchEitherK (toEitherK (Right 1)) (throwEitherK) 
Right 1


--}



{--
2. Short Circuiting Monad Transformer

2.1 runEitherKT :: Monad m => EitherKT e m a -> m (Either e a)
runEitherK (function (params))

2.2 toEitherKT :: Monad m => Either e a -> EitherKT e m a
runEitherKT (toEitherKT (Right 0))
Right 0

2.3 liftEitherK :: Monad m => EitherK e a -> EitherKT e m a
runEitherKT $ liftEitherK (toEitherK (Left 0))
Left 0

2.4 lowerEitherK :: Monad m => EitherKT e m a -> m (EitherK e a)
runEitherKT $ lowerEitherK $ liftEitherK (toEitherK (Left 0))

throw and carch asme as above.
--}