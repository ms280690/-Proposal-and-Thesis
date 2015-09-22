module Unflatten (
  Dict, defaultDict
  , OpType, lookupOp
  , unflatten
  , unflattenList 
  ) where

import qualified Data.Map
import Data.List(foldl',insert)

import Syntax
import PrettyPrint()

type PrologPrio = (OpType,Int)
type Dict = Data.Map.Map String [PrologPrio]

data OpType = Oxf
            | Oyf
            | Ofx
            | Ofy
            | Oxfx
            | Oxfy
            | Oyfx
            | Oyfy
            deriving (Eq,Enum,Ord,Show)

lookupOp :: Dict -> Atom -> [(OpType,Int)]
lookupOp d (Atom s) = Data.Map.findWithDefault [] s d
lookupOp d (Operator s) = Data.Map.findWithDefault [] s d

-- | from http://www.swi-prolog.org/pldoc/man?section=operators
defaultDict :: Dict
defaultDict = makeDict [
    (1200, Oxfx, ["-->", ":-"]),
    (1200, Ofx,  [":-", "?-"]),
    (1150, Ofx,  ["dynamic", "discontiguous", "initialization",
                  "meta_predicate", "module_transparent", "multifile",
                  "thread_local", "volatile"]),
    (1100, Oxfy, [";", "|"]),
    (1050, Oxfy, ["->", "*->"]),
    (1000, Oxfy, ["", ""]),
    (990,  Oxfx, [":="]),
    (900,  Ofy,  ["\\+"]),
    (900,  Ofx,  ["~"]),
    (700,  Oxfx, ["<", "=", "=..", "=@=", "=:=", "=<", "==", "=\\=", ">", ">=",
                  "@<", "@=<", "@>", "@>=", "\\=", "\\==", "is >:<", ":<"]),
    (600,  Oxfy, [":"]),
    (500,  Oyfx, ["+", "-", "/\\", "\\/", "xor"]),
    (500,  Ofx,  ["?"]),
    (400,  Oyfx, ["*", "/", "//", "rdiv", "<<", ">>", "mod", "rem"]),
    (200,  Oxfx, ["**"]),
    (200,  Oxfy, ["^"]),
    (200,  Ofy,  ["+", "-", "\\"])
    ] where 
  makeDict = foldl' addEntry Data.Map.empty
  addEntry :: Dict -> (Int,OpType,[String]) -> Dict
  addEntry dict (prio,op,names) = foldl' (addEntry2 prio op) dict names
  addEntry2 :: Int -> OpType -> Dict -> String -> Dict
  addEntry2 prio op dict name = addPairToEntry name (op,prio) dict
  addPairToEntry :: String -> (OpType, Int) -> Dict -> Dict
  addPairToEntry key listEntry = Data.Map.alter (Just . insertOrMake listEntry) key
  insertOrMake :: Ord a => a -> Maybe [a] -> [a]
  insertOrMake entry = maybe [entry] (Data.List.insert entry) 

data UnpackedFlatItem = UOp {
                           uAtom::Atom,
                           uPre:: Maybe PrologPrio,
                           uInf:: Maybe PrologPrio,
                           uPos:: Maybe PrologPrio}
                      | UTerm Term
   deriving Show

noneOp :: UnpackedFlatItem
noneOp = UOp {uPre=Nothing,uInf=Nothing,uPos=Nothing}

addOp :: PrologPrio -> UnpackedFlatItem -> UnpackedFlatItem
addOp _   y@(UTerm _) = y
addOp t@(Oxf , _) uOp = uOp{uPos=Just t}
addOp t@(Oyf , _) uOp = uOp{uPos=Just t}
addOp t@(Ofx , _) uOp = uOp{uPre=Just t}
addOp t@(Ofy , _) uOp = uOp{uPre=Just t}
addOp t@(Oxfx, _) uOp = uOp{uInf=Just t}
addOp t@(Oxfy, _) uOp = uOp{uInf=Just t}
addOp t@(Oyfx, _) uOp = uOp{uInf=Just t}
addOp t@(Oyfy, _) uOp = uOp{uInf=Just t}

unpack :: Dict -> FlatItem -> UnpackedFlatItem
unpack dict item = case item of
  Bracket list  -> UTerm . unflattenList dict $ list
  FITerm  term  -> UTerm term
  FIOperator op -> foldr addOp (noneOp{uAtom=op}) . lookupOp dict $ op

{- how do i do this generically ?? -}
unflatten :: Dict -> Term -> Term
unflatten d (Flat list) = unflattenList d list
unflatten _ x           = x

unflattenList :: Dict -> [FlatItem] -> Term
unflattenList dict = go1 [(0,id,1000)] . map (unpack dict) where
  missingTermError = error "Syntax: Term expected, but not found"
  termAfterTerm   = error "Syntax: terms not separated \
            \by binary operator"
  prefixAfterTerm = error "Syntax: prefix operator\
                              \ immediately following term"
  badPriorityPrefix = error "Syntax: prefix operator \
         \illegal in context"
  badPriorityPostfix = error "Syntax: postfix operator \
         \illegal in context"
  nonPrefixError = error "Syntax: non-prefix operator found where\
           \ prefix operator expected"

  nullary x   = Struct x []
  infiX x y z = Struct x [y,z]
  prefix x y  = Struct x [y]
  postfix     = prefix
  
  
  -- go1 : we haven't yet seen a term, but we have 1 or more
  --       prefix operations that we can apply, should we see one
  go1 :: [(Int,Term->Term,Int)] -> [UnpackedFlatItem] -> Term
  go1 []       list       = termAfterTerm
  go1 prefixes []         = missingTermError
  go1 prefixes list 
    | (item:rest) <- list = case item of
      UTerm term        -> go2 prefixes  (0, term)        rest
      UOp {uPre=Nothing,
           uAtom=a@(Atom _)} -> let
                                term = Struct a []
                                in go2 prefixes  (0, term)        rest
      UOp {uPre=Nothing}     -> nonPrefixError
      UOp {uPre=Just(op,m2)} -> if priortyCheck
                               then go1 (newPfx:prefixes) rest
                               else badPriorityPrefix
        where 
          newPfx = (m2,prefix . uAtom $ item,m3)
          m3 = case op of { Ofx -> m2-1; _ -> m2 }
          (_,_,oldM):_ = prefixes
          priortyCheck = oldM >= m2

  go2Infix :: [(Int,Term->Term,Int)] -> (Int,Term) -> [UnpackedFlatItem] -> Term
  go2Infix []       (prio, term) list = go5 (prio, term) list
  go2Infix prefixes (prio, term) []   = go4 prefixes (prio, term)
  go2Infix prefixes@((mp1,mpf,mp1a):prefixes') (prio, term) list@(o2@(UOp{}):list')
    | Just (op2,m2) <- uInf o2,
      prio <= mp1a && mp1<=m2 = go2Infix prefixes' (mp1,mpf term) list
    | Just (op2,m2) <- uInf o2,
      prio <= m2 && m2<=mp1a = go1 (nPrefix:prefixes)  list' where
                Just (op2,m2) = uInf o2
                nPrefix = (m2,infiX (uAtom o2) term,m2a)
                m2a = case op2 of
                    Oxfx -> m2-1
                    Oyfx -> m2-1
                    _    -> m2
                
  
  go2 :: [(Int,Term->Term,Int)] -> (Int,Term) -> [UnpackedFlatItem] -> Term
  go2 []       (prio, term) list = go5 (prio, term) list
  go2 prefixes (prio, term) []   = go4 prefixes (prio, term)
  go2 prefixes (prio, term) ((UTerm t2):_) =
        error . ("go2: Unexpected term: "++) . show $ t2
  go2 prefixes@((mp1,mpf,mp1a):prefixes') (prio, term) list@(o2@(UOp {}):list')
    | prio > mp1a             = badPriorityPrefix
    -- prefer infix to postfix meaning if followed by atom or prefix
    | Just _ <- uInf o2,
      UTerm _:_     <- list'  = go2Infix prefixes (prio, term) list
    | Just _ <- uInf o2,
      UOp {uPre=Just _}:_ <- list'
                             = go2Infix prefixes (prio, term) list
    | Just _ <- uInf o2,
      UOp {uAtom=x@(Atom _)}:_ <- list'
                             = let 
                                  list2 = o2:a:tail list'
                                  a = UTerm $ Struct x []
                               in go2Infix prefixes (prio, term) list2
    | Just (_,m2) <- uPos o2,
      prio > m2               = badPriorityPostfix
    | Just (op2,m2) <- uPos o2,
      Oxf <- op2,
      prio >= m2              = badPriorityPostfix
    | Just (op2,m2) <- uPos o2= let
         (newTermA,mA) = (mpf term,mp1)
         (newTermZ,mZ) = (postfix (uAtom o2) term,m2)
         dispatchA     = go2 prefixes' (mA,newTermA) list
         dispatchZ     = go2 prefixes  (mZ,newTermZ) list'
         in if mp1a >= m2 then dispatchZ
            else if mp1 <= (case op2 of Oyf -> m2; _ -> m2-1)
            then dispatchA
            else badPriorityPrefix
    | otherwise             = let
           message = "Unexpected: "++ (show o2) ++ ", followed by " ++
                     show list'
        in error message


  -- go4 : we are wrapping up.  There are no more unprocessed
  --       UnpackedFlatItem's, but there may still be undigested 
  --       prefix operations that we need to apply.
  go4 :: [(Int,Term->Term,Int)] -> (Int,Term) -> Term
  go4 [] (_,t) = t
  go4 ((m,f,__):rest) (n,t)
      |  n<=m       = go4 rest (m, f t)
      |  otherwise  = badPriorityPrefix

  -- go5 : we have seen a term and dealt with the current prefix operations,
  --       but there are still unprocessed items.
  go5 :: (Int,Term) -> [UnpackedFlatItem] -> Term
  go5 (m,t) (o1@UOp {}:UTerm t2:rest)
      | Just (op,m2) <- uInf o1         = case (op,m<m2,m<=m2) of
            (Oyfy,_,True)   -> dispatch o1 m2 rest $! m2
            (Oxfy,True,_)   -> dispatch o1 m2 rest $! m2
            (Oyfx,_,True)   -> dispatch o1 m2 rest $! m2-1 
            (Oxfx,True,_)   -> dispatch o1 m2 rest $! m2-1 
            _               -> badPriorityPrefix
    where 
        dispatch op m2 [] r = go4 [(m2,infiX (uAtom op) t,r)] (0,t2) 
        dispatch op m2 xs r = go2 [(m2,infiX (uAtom op) t,r)] (0,t2) xs

  go5 (m,t) (o1@(UOp {}):rest)
      | Just (op,m2) <- uInf o1         = let
             dispatch [] r = missingTermError
             dispatch xs r = go1 [(m2,infiX (uAtom o1) t,r)] xs
             in case (op,m<m2,m<=m2) of
                (Oyfy,_,True)   -> dispatch rest $! m2
                (Oxfy,True,_)   -> dispatch rest $! m2
                (Oyfx,_,True)   -> dispatch rest $! m2-1 
                (Oxfx,True,_)   -> dispatch rest $! m2-1 
                _               -> badPriorityPrefix
      | Just (op,m2) <- uPos o1         = let
            newTerm       = postfix (uAtom o1) t
            dispatch [] _ = newTerm
            dispatch xs r = go5 (r,newTerm) xs
            in case (op,m<m2,m<=m2) of
                (Oyf,_,True)    -> dispatch rest $! m2
                (Oxf,True,_)    -> dispatch rest $! m2-1 
                _               -> badPriorityPostfix

