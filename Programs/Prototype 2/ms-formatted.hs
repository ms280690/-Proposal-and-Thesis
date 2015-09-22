monadicUnification ::
  (BindingMonad FTS (STVar s FTS) Maybe)
  => Term -> Term
  -> ErrorT
        (UT.UFailure (FTS) (ST.STVar s (FTS)))
        (ST.STBinding s)
        (UT.UTerm (FTS) (ST.STVar s (FTS)), Map Id (ST.STVar s (FTS)))
monadicUnification t1 t2 = do
  let
    t1f = termFlattener t1
    t2f = termFlattener t2
  (x1,d1) <- lift . translateToUTerm $ t1f
  (x2,d2) <- lift . translateToUTerm $ t2f
  x3 <- U.unify x1 x2
  --get state from somehwere, state -> dict
  return $! (x3, d1 `Map.union` d2)


goUnify ::
  (BindingMonad FTS (STVar s FTS) Maybe)
  => (forall s .
       (ErrorT (UT.UFailure (FTS) (ST.STVar s (FTS)))
               (ST.STBinding s)
               (UT.UTerm (FTS) (ST.STVar s (FTS)), Map Id (ST.STVar s (FTS)))
       )
     )
  -> [(Id, Term)]
goUnify test = ST.runSTBinding $ do
  answer <- runErrorT $ test
  return $! case answer of
    (Left x)              -> []
    (Right (_, dict))     -> f1 dict --ERROR
    --[unVar $ unFlatten $ unP $ convertToSubst y]


f1 ::
  (BindingMonad FTS (STVar s FTS) Maybe)
  => Map Id (STVar s FTS) -> [(Id, Term)]
f1 dict =
  Prelude.map (\(k,v) -> (k, unFlatten $ unP $ translateFromUTerm dict v))
              (Prelude.map f2 (Map.toList dict))

f2 :: (BindingMonad t1 v Maybe) => (t, v) -> (t, UTerm t1 v)
f2 (k,v) = (k, fromJust $ fromJust $ UT.lookupVar v)
