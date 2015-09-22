

% Sample Prolog File.

myList(['a', 'A', - 12.3]).

myString('a + b + 3').

append([],X,X).
append([H1| T1], L2,[H1| T2]) :- append(T1, L2, T2).


len([],0).
len([_|T],N) :- len(T,X), N is X + 1.


dcgSentence --> 
				dcgNounPhrase,
				dcgVerbPhrase.
				
dcgNounPhrase --> dcgDeterminer,dcgNoun.

dcgVerbPhrase --> dcgVerb,dcgNounPhrase.

dcgVerbPhrase --> dcgVerb.

dcgDeterminer --> [the].

dcgDeterminer --> [a].

dcgNoun --> [woman].

dcgNoun --> [man].

dcgVerb --> [shoots].


/*

consult "sample_prolog.pl"
Right myList(.(a,.(A,.(<{< -,12.3 >}>,[])))).

myString(a + b + 3).

append([],X,X).

append(.(H1,T1),L2,.(H1,T2)) :- append(T1,L2,T2).

len([],0).

len(.(_,T),N) :- len(T,X), <{< N,is,X,+,1 >}>.

dcgSentence(d_dcgSentence0,d_dcgSentence2) :- dcgNounPhrase(d_dcgSentence0,d_dcgSentence1), dcgVerbPhrase(d_dcgSentence1,d_dcgSentence2).

dcgNounPhrase(d_dcgNounPhrase0,d_dcgNounPhrase2) :- dcgDeterminer(d_dcgNounPhrase0,d_dcgNounPhrase1), dcgNoun(d_dcgNounPhrase1,d_dcgNounPhrase2).

dcgVerbPhrase(d_dcgVerbPhrase0,d_dcgVerbPhrase2) :- dcgVerb(d_dcgVerbPhrase0,d_dcgVerbPhrase1), dcgNounPhrase(d_dcgVerbPhrase1,d_dcgVerbPhrase2).

dcgVerbPhrase(d_dcgVerbPhrase0,d_dcgVerbPhrase1) :- dcgVerb(d_dcgVerbPhrase0,d_dcgVerbPhrase1).

dcgDeterminer(d_dcgDeterminer0,d_dcgDeterminer1) :- .(the,[],d_dcgDeterminer0,d_dcgDeterminer1).

dcgDeterminer(d_dcgDeterminer0,d_dcgDeterminer1) :- .(a,[],d_dcgDeterminer0,d_dcgDeterminer1).

dcgNoun(d_dcgNoun0,d_dcgNoun1) :- .(woman,[],d_dcgNoun0,d_dcgNoun1).

dcgNoun(d_dcgNoun0,d_dcgNoun1) :- .(man,[],d_dcgNoun0,d_dcgNoun1).

dcgVerb(d_dcgVerb0,d_dcgVerb1) :- .(shoots,[],d_dcgVerb0,d_dcgVerb1).

*/
