%% For Question 1

append(nil,L,L). 
append(cons(X,L),M,cons(X,N)) :- append(L,M,N).



%% For Question 2

%%
%% 3 | X . X .
%% 2 | . . X .
%% 1 | . X . .
%% 0 | . . . .
%%   ---------
%%     0 1 2 3
%%

%% 'c' for 'coordinate'.
clear(c(0,0)).
clear(c(1,0)).
clear(c(2,0)).
clear(c(3,0)).
clear(c(0,1)).
clear(c(2,1)).
clear(c(3,1)).
clear(c(0,2)).
clear(c(1,2)).
clear(c(3,2)).
clear(c(1,3)).
clear(c(3,3)).

move1up(0,1).
move1up(1,2).
move1up(2,3).
move1(X,Y) :- move1up(X,Y).
move1(X,Y) :- move1up(Y,X).
move(c(X1,Y), c(X2,Y)) :- move1(X1,X2).
move(c(X,Y1), c(X,Y2)) :- move1(Y1,Y2).

path(P,P,cons(P,nil)).
path(P1,P2,cons(P1,L)) :- move(P1,P3), clear(P3), path(P3,P2,L).

solution(L) :- path(c(0,0), c(3,3), L).
    


%% For question 3.

equal(L,L).

isarithex(var(X)).
isarithex(neg(E)) :- isarithex(E).
isarithex(plus(E1,E2)) :- isarithex(E1), isarithex(E2).

%%vars(X, cons(X, nil)).
%%vars(neg(E),R) :- vars(E,R).
%%vars(plus(E1, E2), R) :- vars(E,R). 
%%vars(plus(E1, E2), R) :- vars (E1,R).
%%vars(plus(E1, E2), R) :- vars(E2, E2wars).
%%vars(plus(E1, E2), R) :- append(E1Var, E2Var, R).

varstest :-
    equal(Input, plus(neg(var(x)), plus(var(y), var(z)))),
    equal(Output, cons(x,cons(y,cons(z,nil)))),
    vars(Input, Output).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


              