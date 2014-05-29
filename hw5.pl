% Exercise 1
when(275,10).
when(261,12).
when(381,11).
when(398,12).
when(399,12).
where(275,owen102).
where(261,dear118).
where(381,cov216).
where(398,cov216).
where(399,dear118).
enroll(mary,275).
enroll(john,275).
enroll(mary,261).
enroll(john,381).
enroll(jim,398).

% (a)
schedule(S,P,T) :- enroll(S,C), (where(C,P), when(C,T), nl).

% (b)
usage(P,T) :- when(C,T), where(C,P), nl.

% (c)
conflict(X,Y) :- when(X,T), when(Y,T), where(X,P), where(Y,P), X \= Y.

% (d)
meet(A,B) :- enroll(A,CA), enroll(B,CB), A \= B, where(CA,P), where(CB,P), when(CA,T1), when(CB,T2), (T1 is T2 + 1; meet(B,A)).
