module EventOracle where

import IOExts
import Unsafe
import System (getProgName)


type Ref = IORef Node
data Node = Node Ref Cost Ref | Marker | Collapsed
type Cost = Int

initialize :: (Ref -> IO a) -> IO ()
initialize app = do
    mod <- getProgName
    let extFile = mod++".ext"
    setAssoc "extfn" extFile
    writeFile extFile ""
    startMarker <- newIORef (error "startMarker")
    endMarker <- newIORef (error "endMarker")
    mainR <- newIORef (Node startMarker 0 endMarker)
    marker <- newIORef Marker
    writeIORef startMarker (Node marker 0 mainR)
    writeIORef endMarker (Node mainR 0 marker)
    x <- app mainR
    return Prelude.$!! x
    putStrLn "finalizing"
    finalize mod startMarker endMarker
    return ()

--- Signals the end of the computation.
finalize :: String -> Ref -> Ref -> IO ()
finalize mod fmR endR = do
    _:l <- pointerToList fmR endR
    writeFile (mod++".steps") (show l ++ "\n.\n")

--- Side effect that computes a fresh reference.
fresh :: () -> Ref  -- must not be a constant in Haskell
fresh _ = unsafePerformIO (newIORef (error "fresh"))

--- increase counter of ref by one
replace :: Ref -> a -> a
replace ref x = unsafePerformIO (do
    --putChar '.'
    node <- readIORef ref
    case node of
      Collapsed -> warning "tried to replace collapsed ref"
      Node p c s -> writeIORef ref Prelude.$!! (Node p (c+1) s)
    return x)

--- Remove a ref and combine and counter +1
collapse :: Ref -> a -> a
collapse ref x = unsafePerformIO (do
    --putChar '%'
    node <- readIORef ref
    case node of
      Collapsed -> warning "tried to collapse collapsed ref"
      Node p c s -> do
        Node pp pc _ <- readIORef p
        Node _ sc ss <- readIORef s
        writeIORef ref Collapsed
        writeIORef p (Node pp pc s)
        writeIORef s Prelude.$!! (Node p (c+1+sc) ss)
    return x)

--- Remove a ref and combine without increment 
--  (used in partCons, partCall only)
closeRef :: Ref -> a -> a
closeRef ref x = unsafePerformIO (do
    --putChar '!'
    node <- readIORef ref
    case node of
      Collapsed -> warning "tried to collapse collapsed ref"
      Node p c s -> do
        Node pp pc _ <- readIORef p
        Node _ sc ss <- readIORef s
        writeIORef ref Collapsed
        writeIORef p (Node pp pc s)
        writeIORef s Prelude.$!! (Node p (c+sc) ss)
    return x)


--- Projection on last argument that releases an event as a side effect.
-- increment step counter of first ref and add remaining refs
-- list has to be at least of size 1
expand :: Ref -> [Ref] -> a -> a
expand ref refs x = unsafePerformIO (do
    --putChar ':'
    --putStr (show (length refs))
    node <- readIORef ref
    case node of
      Collapsed -> warning "tried to expand collapsed ref"
      Node p c s -> do
        last <- (toAssocList Prelude.$!! (c+1)) p (ref:refs) s
        suc <- readIORef s
        case suc of
          Collapsed -> warning "successor of expanded ref is already collapsed"
          Node _ sc ss -> writeIORef s (Node last sc ss)        
          Marker -> done
    return x)

toAssocList :: Cost -> Ref -> [Ref] -> Ref -> IO Ref
toAssocList c p [r] s = writeIORef r (Node p c s) >> return r
toAssocList c p (r:rs@(r':_)) s = do
    writeIORef r (Node p c r')
    toAssocList 0 r rs s

pointerToList :: Ref -> Ref -> IO [Cost]
pointerToList ref end = do
  node <- readIORef ref
  case node of
    Collapsed -> warning "final list contains collapsed reference" >> return []
    Node _ c s -> do
      cs <- pointerToList s end
      return (c:cs)
    Marker -> return []

warning :: String -> IO ()
warning msg = putStrLn $ "WARNING: " ++ msg
