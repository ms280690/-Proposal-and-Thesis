module Curry.Debugger.Bubble 
  (bubble,bubbleWithStore,
   cleanTerm,cleanTermWithStore,
   value) where

import Prelude  as P
import Data.Map as M
import Data.Maybe 

import Curry.Debugger.DebugInfo
import Curry.Debugger.Logic

--------------------------------------------------------
-- remove all unreachable sub branches,
-- e.g., eliminate ?1 below ?1
--------------------------------------------------------

cleanTerm :: Term -> Term
cleanTerm = cleanTermWithStore emptyConstraintStore

-- perhapse cleanTerm should also report on ors included
cleanTermWithStore :: ConstraintStore -> Term -> Term
cleanTermWithStore refMap (Term n si ts) = 
  Term n si (P.map (cleanTermWithStore refMap) ts)
cleanTermWithStore refMap (TermOr si r ts) =   
  case lookupRef refMap r of
   Just i  -> cleanTermWithStore refMap (ts !! i)
   Nothing -> TermOr si r (cleanSequence (insertBind refMap r) ts 0)
cleanTermWithStore _ t = t


cleanSequence :: (Int -> ConstraintStore) -> [Term] -> Int -> [Term]
cleanSequence _ [] _     = []
cleanSequence adjMap (t:ts) i = 
  cleanTermWithStore  (adjMap i) t : 
  (cleanSequence adjMap ts $! i+1)

--------------------------------------------------------
-- remove failures (in bubbled terms)
--------------------------------------------------------

value :: Term -> Maybe Term
value (Term n si ts) = do
  ts' <- mapM value ts
  return (Term n si ts')
value (TermOr si r ts) = do
  case catMaybes $ P.map value ts of
    []  -> Nothing
    [t] -> Just t
    ts' -> Just (TermOr si r ts')
value (TermFail _) = Nothing
value t = Just t

--------------------------------------------------------
-- extend terms with information about references below
--------------------------------------------------------

type OrCollect = Map OrRef Int

data Bubble = BubbleTerm {static::StaticInfo, 
                          refsBelow::OrCollect,
                          refsToBubble::OrCollect,
                          toBubbleBelow::Bool,
                          name::String,
                          args::[Bubble]}
            | BubbleOr   {static::StaticInfo,
                          orRef::OrRef,
                          refsBelow::OrCollect,
                          toBubbleBelow::Bool,
                          args::[Bubble]}
            | Bubbled Term
            deriving (Show,Eq)

allRefsBelow :: Bubble -> OrCollect
allRefsBelow (Bubbled _) = empty
allRefsBelow BubbleOr{orRef=r,refsBelow=refs} = insert r 1 refs
allRefsBelow BubbleTerm{refsBelow=refs} = refs

unionsOC :: [OrCollect] -> OrCollect
unionsOC = unionsWith (\_ _ -> 1)

-- collect all OrRefs below
addRefsToParent :: Term -> (Bubble,OrCollect,Bool)
addRefsToParent t@(Term n si ts) = 
  case unzip3 (P.map addRefsToParent ts) of
    (bs,maps,bubbleBelows) -> 
      let map=unionsWith (+) maps 
          toBubble = M.filter (>1) map 
          set = M.map (const 1) map 
          bubbleBelow = or ((toBubble /= empty) : bubbleBelows)
      in
      if map==empty 
      then (Bubbled t,map,False)
      else (BubbleTerm{static=si,
                       refsBelow=set,
                       refsToBubble=toBubble,
                       toBubbleBelow=bubbleBelow,
                       name=n,
                       args=bs},set,bubbleBelow)
addRefsToParent t@(TermOr si r ts) = 
  case unzip3 (P.map addRefsToParent ts) of
    (bs,maps,bubb) -> 
      let set=unionsOC maps
          bubbleBelow = or bubb
      in
      (BubbleOr{static=si,
                orRef=r,
                refsBelow=set, 
                toBubbleBelow=bubbleBelow,
                args=bs},insert r 1 set,bubbleBelow)
addRefsToParent t = (Bubbled t,empty,False)

addRefs :: Term -> Bubble
addRefs t = case addRefsToParent t of (t',_,_) -> t'

--------------------------------------------------------
-- bubble up a set of references 
--------------------------------------------------------

bubbleUp :: OrCollect -> Bubble -> Bubble
bubbleUp _ t@(Bubbled _) = t
bubbleUp set t@(BubbleTerm{refsBelow=refs,refsToBubble=bset}) 
  | toBubbleBelow t || intersection set refs /= empty
  = let bubbleSet = union bset set
     in merge (\ bs -> 
                  let below = unionsOC (P.map allRefsBelow bs) in
                  if below==empty
                  then Bubbled (Term (name t) (static t) 
                                     (P.map (\ (Bubbled t') -> t') bs))
                  else t{refsBelow=below,
                         refsToBubble=empty,
                         toBubbleBelow=False,
                         args=bs})
              Nothing
              bubbleSet
              (P.map (bubbleUp bubbleSet) (args t))
  | otherwise = t
bubbleUp set t@(BubbleOr{refsBelow=refs,orRef=r})
  | toBubbleBelow t || bubbleSet /= empty
  = let bubbledArgs = P.map (bubbleUp bubbleSet) (args t)
     in if   bubbleSet == empty || member r set
        then t{toBubbleBelow=False,args=bubbledArgs}
        else merge (\ bs -> 
                        t{refsBelow=unionsOC (P.map allRefsBelow bs),
                          toBubbleBelow=False,
                          args=bs})
                   (Just r)
                   bubbleSet
                   bubbledArgs
  | otherwise = t
  where
    bubbleSet = intersection set refs


merge :: ([Bubble] -> Bubble) -> Maybe OrRef
      -> OrCollect -> [Bubble] -> Bubble
merge parent parentRef set args = case nextOr set args of
  Nothing -> parent args 
  Just (si,r,size)  -> 
    let bubbledArgs = P.map (merge parent parentRef (delete r set)) 
                            (makeBranches r size args)
       in
       BubbleOr{static=si,
                orRef=r,
                refsBelow=unionsOC (P.map allRefsBelow bubbledArgs),
                toBubbleBelow=False,
                args=bubbledArgs}


nextOr :: OrCollect -> [Bubble] -> Maybe (StaticInfo,OrRef,Int)
nextOr _   [] = Nothing
nextOr set (o@(BubbleOr{orRef=r,refsBelow=below}):xs) 
  | member r set = Just (static o,r,length (args o))
  | hiddenRefs /= empty = nextOr set (bubbleUp hiddenRefs o:xs)
  | otherwise    = nextOr set xs
  where
    hiddenRefs = intersection below set
nextOr set (_:xs) = nextOr set xs

makeBranches :: OrRef -> Int -> [Bubble] -> [[Bubble]]
makeBranches _ i [] = replicate i []
makeBranches r i (t@(BubbleOr{orRef=r',refsBelow=refs,args=bs}):args) 
  | r==r' = zipWith (:) bs (makeBranches r i args)
  | member r refs 
  = makeBranches r i (bubbleUp (singleton r 1) t:args) 
makeBranches r i (t:args) = P.map (t:) (makeBranches r i args)


bubble :: Term -> Term
bubble = bubbleWithStore emptyConstraintStore

bubbleWithStore :: ConstraintStore -> Term -> Term
bubbleWithStore store t = 
  deBubble $ case addRefsToParent (cleanTermWithStore store t) of
               (t',_,True)  -> bubbleUp empty t'
               (t',_,False) -> t'


deBubble :: Bubble -> Term
deBubble BubbleTerm{static=si,name=n,args=ts} = Term n si (P.map deBubble ts)
deBubble BubbleOr{static=si,orRef=r,args=ts}  = TermOr si r (P.map deBubble ts)
deBubble (Bubbled t) = t

