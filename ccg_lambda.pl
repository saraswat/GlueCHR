:- module(ccg, [query/3,w/3,apply/3,normalize/2]).
:- use_module(library(chr)).

:-chr_constraint v/4, s/3, e/1, w/3.

:- op(400, xfy, user:(\)).

% Choose * for application because it is at the right priority level
% and is left associative.
% Choose ^ for lambda binder, since it is tighter than * and left assoc.

v(I,J,L,S), v(J,K,Z\L1,T) <=> \+ (\+ L=L1) | L=L1,apply(T,S,U), v(I,K,Z,U).
v(I,J,Z/R,S), v(J,K,R1,T) <=> \+ (\+ R=R1) | R=R1,apply(S,T,U), v(I,K,Z,U).


s(I,S,M), v(I,J,S1,M1), e(J) <=> S=S1, M=M1.

w(refused,I,J) <=>                v(I,J,(s\np)/(s\np), P^X^E^and([refuse*E, a0*X*E, exists(E1^and([P*X*E1,a1*E1*E]))])).
w(to,I,J)      <=>                v(I,J,(s\np)/(s\np), P^X^E^(P*X*E)).
w(or,I,J)      <=>                v(I,J,(Z\Z)/Z,       Q^P^X^Y^E^or([P*X*Y*E,Q*X*Y*E])).
w(V,I,J)       <=> trans_verb(V)| v(I,J,(s\np)/np,     X^Y^E^and([V*E, ao*Y*E, a1*X*E])).
w(V,I,J)       <=> np(V)        | v(I,J,np,            V).


query(L, S, M) :- s(1,S,M), run(L, 1, N), e(N).
run([], N,N).
run([A|X], M, N):- M1 is M+1, w(A, M, M1), run(X,M1,N).

trans_verb(loves).
trans_verb(confirm).
trans_verb(deny).

np(he).
np(ed).
np(ann).
np(bill).
np(hillary).
np(reports).

% An implementation of lambda terms as first order terms.
% lambda variables are represented as first order variables.
% In any call apply(A, B, C), A and B must be bound to lambda terms
% in which all bound variables are distinct.

apply(X^B,C, D):- subst(C, X, B, D1), normalize(D1, D).

% subst(C, X, Y, D):- D is obtained by substituting C for X in Y.
subst(C, X, Y, D):- atomic(Y), !, (X==Y, C=D; X\==Y, Y=D).
subst(C, X, Y, D):- var(Y),    !, (X==Y, C=D; X\==Y, Y=D).
subst(C, X, Z^Y, Z^D1)             :- subst(C, X, Y, D1).
subst(C, X, Y1*Y2, D1*D2)          :- subst(C, X, Y1, D1), subst(C, X, Y2, D2).
subst(C, X, [Y1 | Y2], [D1|D2])    :- subst(C, X, Y1, D1), subst(C, X, Y2, D2).
subst(C, X, and(Y), and(D))        :- subst(C, X, Y, D).
subst(C, X, or(Y), or(D))          :- subst(C, X, Y, D).
subst(C, X, exists(Y), exists(D))  :- subst(C, X, Y, D).
	
normalize(X, Y) :- atomic(X), !, X=Y.
normalize(X, Y) :- var(X), !, X=Y.
normalize(X^Y, X^D)            :- normalize(Y, D).
normalize(A*C, D)              :- normalize(A, A1), normalize(A1, C, D).
normalize(and(X), and(Y))      :- normalize(X,Y).
normalize(or(X), or(Y))        :- normalize(X,Y).
normalize(exists(X), exists(Y)):- normalize(X,Y).
normalize([], []).
normalize([X|Xs], [Y|Ys]):- normalize(X,Y), normalize(Xs, Ys).

normalize(A, C, A*C1):- atomic(A), !, normalize(C, C1).
normalize(A, C, A*C1):- var(A), !, normalize(C, C1).
normalize(X^B, C, D):- apply(X^B, C, D).
normalize(A*B, C, A*B*C).
