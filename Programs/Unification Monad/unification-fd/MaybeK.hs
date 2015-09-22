

-- Mehul Solanki.

-- Maybek from unification-fd.


import Control.Monad.MaybeK
import Control.Monad.IO.Class
{-- 
1. The Partiality Monad

CPS style Maybe based on Church encoding ?????
Short circuiting at failure.

Right associative binds.

No need for intermediate csae expressions.

--} 

{--
toMaybeK :: Maybe a -> MaybeK a

runMaybeK :: MaybeK a -> Maybe a

runMaybeK (toMaybeK (Just 5))
Just 5
--}


{--
maybeK
Somthing like maybe,

maybeK (Just 5) Just (toMaybeK (Just 4))
Just 4

maybeK (Just 5) Just (toMaybeK Nothing)
Just 5
--}


{--
2. The Partitality Monad Transformer

2.1 For executing the statements 
runMaybeKT :: Monad m => MaybeKT m a -> m (Maybe a)

runMaybeKT (function (params))

2.2 Maybe to MaybeKT
toMaybeKT :: Monad m => Maybe a -> MaybeKT m a
runMaybeKT (toMaybeKT (Just 1))
Just 1

2.3 MaybeK to MaybeKT
liftMaybeK :: Monad m => MaybeK a -> MaybeKT m a
runMaybeKT (liftMaybeK (toMaybeK (Just 1)))
Just 1


2.4 MaybeKT to MaybeK
lowerMaybeK :: Monad m => MaybeKT m a -> m (MaybeK a)
lowerMaybeK $ runMaybeKT (liftMaybeK (toMaybeK (Just 1))) 

--}



main = do
	x <- liftIO $ lowerMaybeK $ runMaybeKT (liftMaybeK (toMaybeK (Just 1)))
	return x
	print 1