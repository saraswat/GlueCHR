:- module(ccg, [query/3,w/3,apply/3,normalize/2,e/3]).
:- use_module(library(chr)).
:-reexport([ccg_vocab]).
:-chr_constraint v/4, s/3, e/1, w/3.

:- op(400, xfy, user:(\)).

query(U,S,M) :- s(L,S,M), run(U,L,R), e(R).
run([],L,L).
run([[A|As]|X],L,R):- !,query([A|As],Type,Mean), v(L,M,Type,Mean), run(X, M, R).
run([A|X],L,R):- w(A,L,M), run(X,M,R).
s(I,S,M), v(I,J,S1,M1), e(J) <=> S=S1, M=M1.

% Choose * for application because it is at the right priority level
% and is left associative.
% Choose ^ for lambda binder, since it is tighter than * and left assoc.

v(I,J,L1,S), v(J,K,Z\L,T) <=> unifiable(L, L1,_) | L=L1, apply(T,S,U), v(I,K,Z,U).
v(I,J,Z/R,S), v(J,K,R1,T) <=> unifiable(R, R1, _) | R=R1, apply(S,T,U), v(I,K,Z,U).

  % hack to work around CHR bug. Ensures or rule fires only on right.
%v(I,J,Z/R,S), v(J,K,R1,T) <=> var(R) | R=R1, apply(S,T,U), v(I,K,Z,U). 
w(Word, I, J) <=> e(Word, Type, Term) | v(I, J, Type, Term).
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
normalize(A*C, D)              :- normalize(A, A1), normalize(C,C1), normalize(A1, C1, D).
normalize(and([and(X)|O]), Y)  :- !, append(X, O, XO), normalize(and(XO),Y).
normalize(and(X), and(Y))      :- normalize(X,Y).
normalize(or([or(X),O]), Y)    :- !, append(X, O, XO), normalize(or(XO),Y).
normalize(or(X), or(Y))        :- normalize(X,Y).
normalize(exists(X), exists(Y)):- normalize(X,Y).
normalize([], []).
normalize([X|Xs], [Y|Ys]):- normalize(X,Y), normalize(Xs, Ys).

normalize(A, C, A*C1):- atomic(A), !, normalize(C, C1).
normalize(A, C, A*C1):- var(A), !, normalize(C, C1).
normalize(X^B, C, D):- apply(X^B, C, D).
normalize(A*B, C, A*B*C).


%------------- queries
% query([he, refused, to, confirm, or, deny, reports], S, M), write_term(M, [max_depth=20]).
%query([if, roses, [[are, red], then], violets, are, blue], S, M), write_term(M, [max_depth=20]).