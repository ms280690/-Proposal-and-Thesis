module Search where

-- Search space classes
--
-- types:
--  s : search states
--  m : moves
-- 
class SearchProblem s m where
  trans            :: s -> [(m,s)]
  space, solutions :: s -> [([m],s)]
  isSolution       :: ([m],s) -> Bool

  space s = step ++ expand step
            where step = [([m],t) | (m,t) <- trans s]
                  expand ss = [ (ms++ns,t) | (ms,s) <- ss, (ns,t) <- space s ]  
  solutions = filter isSolution . space
                      

