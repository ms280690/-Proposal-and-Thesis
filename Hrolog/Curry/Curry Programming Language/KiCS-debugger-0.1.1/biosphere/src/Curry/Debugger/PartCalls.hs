
module Curry.Debugger.PartCalls where
import qualified Curry.Debugger.DebugMonad as DM
import qualified Curry.Debugger.DebugInfo as DI
 
partCall1 ::
          (DM.DM dm, DI.GenTerm a, DI.GenTerm b) =>
            DI.Term -> (a -> dm b) -> DM.Func dm a b
partCall1 (DI.Term name info args) f
  = DM.FuncRep (term []) (\ x1 -> f x1)
  where term = (DI.Term name info) Prelude.. ((args Prelude.++))
 
partCall2 ::
          (DM.DM dm, DI.GenTerm a, DI.GenTerm b, DI.GenTerm c) =>
            DI.Term -> (a -> b -> dm c) -> DM.Func dm a (DM.Func dm b c)
partCall2 (DI.Term name info args) f
  = DM.FuncRep (term [])
      (\ x1 ->
         do let t1 = DI.genTerm x1
            Prelude.return (DM.FuncRep (term [t1]) (\ x2 -> f x1 x2)))
  where term = (DI.Term name info) Prelude.. ((args Prelude.++))
 
partCall3 ::
          (DM.DM dm, DI.GenTerm a, DI.GenTerm b, DI.GenTerm c,
           DI.GenTerm d) =>
            DI.Term ->
              (a -> b -> c -> dm d) ->
                DM.Func dm a (DM.Func dm b (DM.Func dm c d))
partCall3 (DI.Term name info args) f
  = DM.FuncRep (term [])
      (\ x1 ->
         do let t1 = DI.genTerm x1
            Prelude.return
              (DM.FuncRep (term [t1])
                 (\ x2 ->
                    do let t2 = DI.genTerm x2
                       Prelude.return (DM.FuncRep (term [t1, t2]) (\ x3 -> f x1 x2 x3)))))
  where term = (DI.Term name info) Prelude.. ((args Prelude.++))
 
partCall4 ::
          (DM.DM dm, DI.GenTerm a, DI.GenTerm b, DI.GenTerm c, DI.GenTerm d,
           DI.GenTerm e) =>
            DI.Term ->
              (a -> b -> c -> d -> dm e) ->
                DM.Func dm a (DM.Func dm b (DM.Func dm c (DM.Func dm d e)))
partCall4 (DI.Term name info args) f
  = DM.FuncRep (term [])
      (\ x1 ->
         do let t1 = DI.genTerm x1
            Prelude.return
              (DM.FuncRep (term [t1])
                 (\ x2 ->
                    do let t2 = DI.genTerm x2
                       Prelude.return
                         (DM.FuncRep (term [t1, t2])
                            (\ x3 ->
                               do let t3 = DI.genTerm x3
                                  Prelude.return
                                    (DM.FuncRep (term [t1, t2, t3]) (\ x4 -> f x1 x2 x3 x4)))))))
  where term = (DI.Term name info) Prelude.. ((args Prelude.++))
 
partCall5 ::
          (DM.DM dm, DI.GenTerm a, DI.GenTerm b, DI.GenTerm c, DI.GenTerm d,
           DI.GenTerm e, DI.GenTerm f) =>
            DI.Term ->
              (a -> b -> c -> d -> e -> dm f) ->
                DM.Func dm a
                  (DM.Func dm b (DM.Func dm c (DM.Func dm d (DM.Func dm e f))))
partCall5 (DI.Term name info args) f
  = DM.FuncRep (term [])
      (\ x1 ->
         do let t1 = DI.genTerm x1
            Prelude.return
              (DM.FuncRep (term [t1])
                 (\ x2 ->
                    do let t2 = DI.genTerm x2
                       Prelude.return
                         (DM.FuncRep (term [t1, t2])
                            (\ x3 ->
                               do let t3 = DI.genTerm x3
                                  Prelude.return
                                    (DM.FuncRep (term [t1, t2, t3])
                                       (\ x4 ->
                                          do let t4 = DI.genTerm x4
                                             Prelude.return
                                               (DM.FuncRep (term [t1, t2, t3, t4])
                                                  (\ x5 -> f x1 x2 x3 x4 x5)))))))))
  where term = (DI.Term name info) Prelude.. ((args Prelude.++))
 
partCall6 ::
          (DM.DM dm, DI.GenTerm a, DI.GenTerm b, DI.GenTerm c, DI.GenTerm d,
           DI.GenTerm e, DI.GenTerm f, DI.GenTerm g) =>
            DI.Term ->
              (a -> b -> c -> d -> e -> f -> dm g) ->
                DM.Func dm a
                  (DM.Func dm b
                     (DM.Func dm c (DM.Func dm d (DM.Func dm e (DM.Func dm f g)))))
partCall6 (DI.Term name info args) f
  = DM.FuncRep (term [])
      (\ x1 ->
         do let t1 = DI.genTerm x1
            Prelude.return
              (DM.FuncRep (term [t1])
                 (\ x2 ->
                    do let t2 = DI.genTerm x2
                       Prelude.return
                         (DM.FuncRep (term [t1, t2])
                            (\ x3 ->
                               do let t3 = DI.genTerm x3
                                  Prelude.return
                                    (DM.FuncRep (term [t1, t2, t3])
                                       (\ x4 ->
                                          do let t4 = DI.genTerm x4
                                             Prelude.return
                                               (DM.FuncRep (term [t1, t2, t3, t4])
                                                  (\ x5 ->
                                                     do let t5 = DI.genTerm x5
                                                        Prelude.return
                                                          (DM.FuncRep (term [t1, t2, t3, t4, t5])
                                                             (\ x6 -> f x1 x2 x3 x4 x5 x6)))))))))))
  where term = (DI.Term name info) Prelude.. ((args Prelude.++))
 
partCall7 ::
          (DM.DM dm, DI.GenTerm a, DI.GenTerm b, DI.GenTerm c, DI.GenTerm d,
           DI.GenTerm e, DI.GenTerm f, DI.GenTerm g, DI.GenTerm h) =>
            DI.Term ->
              (a -> b -> c -> d -> e -> f -> g -> dm h) ->
                DM.Func dm a
                  (DM.Func dm b
                     (DM.Func dm c
                        (DM.Func dm d (DM.Func dm e (DM.Func dm f (DM.Func dm g h))))))
partCall7 (DI.Term name info args) f
  = DM.FuncRep (term [])
      (\ x1 ->
         do let t1 = DI.genTerm x1
            Prelude.return
              (DM.FuncRep (term [t1])
                 (\ x2 ->
                    do let t2 = DI.genTerm x2
                       Prelude.return
                         (DM.FuncRep (term [t1, t2])
                            (\ x3 ->
                               do let t3 = DI.genTerm x3
                                  Prelude.return
                                    (DM.FuncRep (term [t1, t2, t3])
                                       (\ x4 ->
                                          do let t4 = DI.genTerm x4
                                             Prelude.return
                                               (DM.FuncRep (term [t1, t2, t3, t4])
                                                  (\ x5 ->
                                                     do let t5 = DI.genTerm x5
                                                        Prelude.return
                                                          (DM.FuncRep (term [t1, t2, t3, t4, t5])
                                                             (\ x6 ->
                                                                do let t6 = DI.genTerm x6
                                                                   Prelude.return
                                                                     (DM.FuncRep
                                                                        (term
                                                                           [t1, t2, t3, t4, t5, t6])
                                                                        (\ x7 ->
                                                                           f x1 x2 x3 x4 x5 x6
                                                                             x7)))))))))))))
  where term = (DI.Term name info) Prelude.. ((args Prelude.++))
 
partCall8 ::
          (DM.DM dm, DI.GenTerm a, DI.GenTerm b, DI.GenTerm c, DI.GenTerm d,
           DI.GenTerm e, DI.GenTerm f, DI.GenTerm g, DI.GenTerm h,
           DI.GenTerm i) =>
            DI.Term ->
              (a -> b -> c -> d -> e -> f -> g -> h -> dm i) ->
                DM.Func dm a
                  (DM.Func dm b
                     (DM.Func dm c
                        (DM.Func dm d
                           (DM.Func dm e (DM.Func dm f (DM.Func dm g (DM.Func dm h i)))))))
partCall8 (DI.Term name info args) f
  = DM.FuncRep (term [])
      (\ x1 ->
         do let t1 = DI.genTerm x1
            Prelude.return
              (DM.FuncRep (term [t1])
                 (\ x2 ->
                    do let t2 = DI.genTerm x2
                       Prelude.return
                         (DM.FuncRep (term [t1, t2])
                            (\ x3 ->
                               do let t3 = DI.genTerm x3
                                  Prelude.return
                                    (DM.FuncRep (term [t1, t2, t3])
                                       (\ x4 ->
                                          do let t4 = DI.genTerm x4
                                             Prelude.return
                                               (DM.FuncRep (term [t1, t2, t3, t4])
                                                  (\ x5 ->
                                                     do let t5 = DI.genTerm x5
                                                        Prelude.return
                                                          (DM.FuncRep (term [t1, t2, t3, t4, t5])
                                                             (\ x6 ->
                                                                do let t6 = DI.genTerm x6
                                                                   Prelude.return
                                                                     (DM.FuncRep
                                                                        (term
                                                                           [t1, t2, t3, t4, t5, t6])
                                                                        (\ x7 ->
                                                                           do let t7 = DI.genTerm x7
                                                                              Prelude.return
                                                                                (DM.FuncRep
                                                                                   (term
                                                                                      [t1, t2, t3,
                                                                                       t4, t5, t6,
                                                                                       t7])
                                                                                   (\ x8 ->
                                                                                      f x1 x2 x3 x4
                                                                                        x5
                                                                                        x6
                                                                                        x7
                                                                                        x8)))))))))))))))
  where term = (DI.Term name info) Prelude.. ((args Prelude.++))
 
partCall9 ::
          (DM.DM dm, DI.GenTerm a, DI.GenTerm b, DI.GenTerm c, DI.GenTerm d,
           DI.GenTerm e, DI.GenTerm f, DI.GenTerm g, DI.GenTerm h,
           DI.GenTerm i, DI.GenTerm j) =>
            DI.Term ->
              (a -> b -> c -> d -> e -> f -> g -> h -> i -> dm j) ->
                DM.Func dm a
                  (DM.Func dm b
                     (DM.Func dm c
                        (DM.Func dm d
                           (DM.Func dm e
                              (DM.Func dm f (DM.Func dm g (DM.Func dm h (DM.Func dm i j))))))))
partCall9 (DI.Term name info args) f
  = DM.FuncRep (term [])
      (\ x1 ->
         do let t1 = DI.genTerm x1
            Prelude.return
              (DM.FuncRep (term [t1])
                 (\ x2 ->
                    do let t2 = DI.genTerm x2
                       Prelude.return
                         (DM.FuncRep (term [t1, t2])
                            (\ x3 ->
                               do let t3 = DI.genTerm x3
                                  Prelude.return
                                    (DM.FuncRep (term [t1, t2, t3])
                                       (\ x4 ->
                                          do let t4 = DI.genTerm x4
                                             Prelude.return
                                               (DM.FuncRep (term [t1, t2, t3, t4])
                                                  (\ x5 ->
                                                     do let t5 = DI.genTerm x5
                                                        Prelude.return
                                                          (DM.FuncRep (term [t1, t2, t3, t4, t5])
                                                             (\ x6 ->
                                                                do let t6 = DI.genTerm x6
                                                                   Prelude.return
                                                                     (DM.FuncRep
                                                                        (term
                                                                           [t1, t2, t3, t4, t5, t6])
                                                                        (\ x7 ->
                                                                           do let t7 = DI.genTerm x7
                                                                              Prelude.return
                                                                                (DM.FuncRep
                                                                                   (term
                                                                                      [t1, t2, t3,
                                                                                       t4, t5, t6,
                                                                                       t7])
                                                                                   (\ x8 ->
                                                                                      do let t8
                                                                                               = DI.genTerm
                                                                                                   x8
                                                                                         Prelude.return
                                                                                           (DM.FuncRep
                                                                                              (term
                                                                                                 [t1,
                                                                                                  t2,
                                                                                                  t3,
                                                                                                  t4,
                                                                                                  t5,
                                                                                                  t6,
                                                                                                  t7,
                                                                                                  t8])
                                                                                              (\ x9
                                                                                                 ->
                                                                                                 f
                                                                                                   x1
                                                                                                   x2
                                                                                                   x3
                                                                                                   x4
                                                                                                   x5
                                                                                                   x6
                                                                                                   x7
                                                                                                   x8
                                                                                                   x9)))))))))))))))))
  where term = (DI.Term name info) Prelude.. ((args Prelude.++))
 
partCall10 ::
           (DM.DM dm, DI.GenTerm a, DI.GenTerm b, DI.GenTerm c, DI.GenTerm d,
            DI.GenTerm e, DI.GenTerm f, DI.GenTerm g, DI.GenTerm h,
            DI.GenTerm i, DI.GenTerm j, DI.GenTerm k) =>
             DI.Term ->
               (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> dm k) ->
                 DM.Func dm a
                   (DM.Func dm b
                      (DM.Func dm c
                         (DM.Func dm d
                            (DM.Func dm e
                               (DM.Func dm f
                                  (DM.Func dm g (DM.Func dm h (DM.Func dm i (DM.Func dm j k)))))))))
partCall10 (DI.Term name info args) f
  = DM.FuncRep (term [])
      (\ x1 ->
         do let t1 = DI.genTerm x1
            Prelude.return
              (DM.FuncRep (term [t1])
                 (\ x2 ->
                    do let t2 = DI.genTerm x2
                       Prelude.return
                         (DM.FuncRep (term [t1, t2])
                            (\ x3 ->
                               do let t3 = DI.genTerm x3
                                  Prelude.return
                                    (DM.FuncRep (term [t1, t2, t3])
                                       (\ x4 ->
                                          do let t4 = DI.genTerm x4
                                             Prelude.return
                                               (DM.FuncRep (term [t1, t2, t3, t4])
                                                  (\ x5 ->
                                                     do let t5 = DI.genTerm x5
                                                        Prelude.return
                                                          (DM.FuncRep (term [t1, t2, t3, t4, t5])
                                                             (\ x6 ->
                                                                do let t6 = DI.genTerm x6
                                                                   Prelude.return
                                                                     (DM.FuncRep
                                                                        (term
                                                                           [t1, t2, t3, t4, t5, t6])
                                                                        (\ x7 ->
                                                                           do let t7 = DI.genTerm x7
                                                                              Prelude.return
                                                                                (DM.FuncRep
                                                                                   (term
                                                                                      [t1, t2, t3,
                                                                                       t4, t5, t6,
                                                                                       t7])
                                                                                   (\ x8 ->
                                                                                      do let t8
                                                                                               = DI.genTerm
                                                                                                   x8
                                                                                         Prelude.return
                                                                                           (DM.FuncRep
                                                                                              (term
                                                                                                 [t1,
                                                                                                  t2,
                                                                                                  t3,
                                                                                                  t4,
                                                                                                  t5,
                                                                                                  t6,
                                                                                                  t7,
                                                                                                  t8])
                                                                                              (\ x9
                                                                                                 ->
                                                                                                 do let t9
                                                                                                          = DI.genTerm
                                                                                                              x9
                                                                                                    Prelude.return
                                                                                                      (DM.FuncRep
                                                                                                         (term
                                                                                                            [t1,
                                                                                                             t2,
                                                                                                             t3,
                                                                                                             t4,
                                                                                                             t5,
                                                                                                             t6,
                                                                                                             t7,
                                                                                                             t8,
                                                                                                             t9])
                                                                                                         (\
                                                                                                            x10
                                                                                                            ->
                                                                                                            f
                                                                                                              x1
                                                                                                              x2
                                                                                                              x3
                                                                                                              x4
                                                                                                              x5
                                                                                                              x6
                                                                                                              x7
                                                                                                              x8
                                                                                                              x9
                                                                                                              x10)))))))))))))))))))
  where term = (DI.Term name info) Prelude.. ((args Prelude.++))