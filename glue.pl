%% An implementation of Glue Logic using Constraint Handling Rules.

:- module(glue, [v/2,v/3,v/4, '~>'/2]).
:- use_module(library(chr)).

:-chr_constraint v/2, v/3, v/4, quant/3, quant/2, '~>'/2.
:- op(700, xfx, user:(~>)).
% bill(g), hillary(h), appointed(g,h,f)
% v(bill, g), v(convinced, g,h,f), v(everyone, h).


v(X, G)                       <=> noun(X,Y)    | G ~> Y.
v(X, G, H, F), G ~> A, H ~> B <=> vp(X, A^B^V) | F ~> V.
v(X, F),       F ~> P         <=> mod(X, P^V)  | F ~> V.
v(X, Rest, H)                 <=> quant(X, Q)  | H ~> X0, quant(Rest, X0, Q).
v(X, H)                       <=> quant1(X, Q) | H ~> X0, quant(X0, Q).

quant(Rest, X, Q), Rest ~> Qm, Scope ~> Y <=> lambda(Y, M, X) | Q=Qm^M^Z, Scope ~> Z.
quant(X, Q), Scope ~> Y <=> lambda(Y, M, X) | Q=M^Z, Scope ~> Z.

%--- Helper predicate lambda/3
% lambda(Y, S, X) :- Y = S(X), but Y must not be X and
% must have at least one occurrence of X.

lambda(Y, X^Y, X):- nonvar(Y), contains(Y, X).

contains(Y, X) :- var(Y), !, Y == X.
contains(Y, X) :- atomic(Y), !, Y == X.
contains([Y | Z], X):- !, (contains(Y, X); contains(Z,X)), !.
contains(Y, X) :- Y=..L, contains(L, X).

% --- vocabulary.
% Enough for the paper "Linear Logic for Meaning Assembly"

noun(candidate, candidate).
noun(manager, manager).
noun(bill, bill).
noun(hillary, hillary).
noun(man, man).
noun(woman, woman).

vp(appointed, X^Y^appointed(X,Y)).
vp(convinced, X^Y^convinced(X,Y)).
vp(loves, X^Y^loves(X,Y)).

mod(obviously, X^obviously(X)).
quant(a, X^Y^a(X,Y)).
quant(some, X^Y^some(X,Y)).
quant(every, X^Y^every(X,Y)).
quant1(everyone, Y^every(person,Y)).
