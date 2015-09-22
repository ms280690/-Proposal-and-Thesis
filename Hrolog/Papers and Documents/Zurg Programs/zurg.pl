/* The Zurg Riddle */

time(buzz,5).
time(woody,10).
time(rex,20).
time(hamm,25).

toys([buzz,hamm,rex,woody]).

cost([],0) :- !.
cost([X|L],C) :- 
     time(X,S), 
     cost(L,D), 
     C is max(S,D).

split(L,[X,Y],M) :- 
     member(X,L),
     member(Y,L),
     compare(<,X,Y),  
     subtract(L,[X,Y],M).

move(st(l,L1),st(r,L2),r(M),D) :- 
     split(L1,M,L2),
     cost(M,D).

move(st(r,L1),st(l,L2),l(X),D) :-
     toys(T),
     subtract(T,L1,R),
     member(X,R),
     merge_set([X],L1,L2),
     time(X,D).

trans(st(r,[]),st(r,[]),[],0).    
trans(S,U,L,D) :-
     move(S,T,M,X),
     trans(T,U,N,Y),
     append([M],N,L),
     D is X + Y.

cross(M,D) :-
     toys(T),
     trans(st(l,T),st(r,[]),M,D0),
     D0=<D.

solution(M) :- cross(M,60).
