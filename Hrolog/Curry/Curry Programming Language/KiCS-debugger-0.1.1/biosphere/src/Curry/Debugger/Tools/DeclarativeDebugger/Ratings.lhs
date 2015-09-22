> module Curry.Debugger.Tools.DeclarativeDebugger.Ratings ( Ratings (..)
>                , Rating (..)
>                , new
>                , shift
>                , replace
>                , push
>                , pop
>                , top
>                , current
>                , restart
>                ) where

> data Ratings = Ratings { past   :: RatingStack
>                        , future :: RatingStack
>                        }
>  deriving Show

> type RatingStack = [RatingBurst]

> data RatingBurst = Burst Rating Int
>  deriving Show

> data Rating = Correct
>             | Wrong
>             | Unrated
>  deriving Eq

> instance Show Rating where
>     showsPrec _prec Correct = showString "correct"
>     showsPrec _prec Wrong   = showString "wrong"
>     showsPrec _prec Unrated = showString "unrated"

> new :: Ratings
> new = Ratings { past   = []
>               , future = []
>               }

> shift :: Ratings -> Int -> Ratings
> shift r@(Ratings p f) n
>     | n == 0 =
>         r
>     | n < 0 =
>         let (Ratings f' p') = shift (Ratings f p) (-n)
>         in  (Ratings p' f')
>     | otherwise =
>         case f of
>             [] ->
>                 Ratings (push p n Unrated) f
>             (b@(Burst r m) : rs)
>                 | n < m ->
>                     Ratings (push p n r) ((Burst r (m - n)) : rs)
>                 | n == m ->
>                     Ratings (b : p) rs
>                 | otherwise ->
>                     shift (Ratings (b : p) rs) $ n - m

> replace :: Ratings -> Int -> Rating -> Ratings
> replace (Ratings p f) n r = let f' = push (pop f n) 1 r
>                          in  Ratings { past = p, future = f' }

> current :: Ratings -> Rating
> current (Ratings _p f) = top f

> restart :: Ratings -> Ratings
> restart (Ratings p f) = Ratings { past = []
>                                 , future = foldl (flip (:)) f p
>                                 }

> top :: RatingStack -> Rating
> top [] = Unrated
> top (Burst r _n : rs) = r

> push :: RatingStack -> Int -> Rating -> RatingStack
> push [] n r = [Burst r n]
> push (b@(Burst rb m) : rs) n r
>     | rb == r   = Burst rb (m + n) : rs
>     | otherwise = Burst r n : b : rs

> pop :: RatingStack -> Int-> RatingStack
> pop [] _n = []
> pop (Burst _r m : rs) n 
>     | m > n     = Burst _r (m - n) : rs
>     | m == n    = rs
>     | otherwise = pop rs $ n - m

