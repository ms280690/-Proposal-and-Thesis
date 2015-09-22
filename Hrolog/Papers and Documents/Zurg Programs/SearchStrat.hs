module SearchStrat where

-- Search space classes
--
-- types:
--  s : search states
--  m : moves
-- 
type Space m s = [([m],s)]
type Strategy m s = Space m s -> Space m s -> Space m s

class SearchProblem s m where
  trans            :: s -> [(m,s)]
  isSolution       :: ([m],s) -> Bool
  space, solutions :: Strategy m s -> s -> Space m s

  space f s = expand f (step ([],s))
              where expand f []     = []
                    expand f (s:ss) = s:expand f (f (step s) ss)
                    step (ms,s)     = [(ms++[m],t) | (m,t) <- trans s]
                
  solutions f = filter isSolution . space f
                      

dfs = (++)
bfs = flip dfs
