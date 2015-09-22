module CEventOracle where

import IOExts (setAssoc)
import System (getProgName)
import Prelude as P

-------------------------
-- external variants 
-------------------------

data Ref

initialize :: (Ref -> IO a) -> IO ()
initialize app = do
    mod <- getProgName
    let extFile = mod++".ext"
    setAssoc "extfn" extFile
    writeFile extFile ""
    mainR <- initRef
    x <- app mainR
    st <- getSearchTree x
    catchFail (do putStrLn "starting to record oracle" 
                  return P.$!! st)
              (putStrLn "execution aborted. Writing oracle file" >>
               return failed)
    finalize (mod++".steps")

initRef :: IO Ref
initRef external


finalize :: String -> IO ()
finalize external

fresh :: () -> Ref  -- must not be a constant in Haskell
fresh external

replace :: Ref -> a -> a
replace external

collapse :: Ref -> a -> a
collapse external

closeRef :: Ref -> a -> a
closeRef external

expand :: Ref -> [Ref] -> a -> a
expand external

--- generating a fresh variable
unknown :: Ref -> a
unknown external

($!),($!!),($#),($##), apply :: (Ref -> a -> b) -> a -> Ref -> b
($!)  external
($!!) external
($#)  external
($##) external
apply external