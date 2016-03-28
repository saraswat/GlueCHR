:- module(ccg, [v/4, c/3, '~>'/2]).
:- use_module(library(chr)).

:-chr_constraint v/2, v/3, v/4, quant/3, quant/2, '~>'/2.
:- op(700, xfx, ~>).
:- op(400, yfx, \).
v(I, J, L, S), v(J,K, L1\R, T) <=> L=L1, v(I, K, R, (S,T)).
v(I, J, L/R, S), v(J,K, R1, T) <=> R=R1, v(I, K, R, (S,T)).

reports: np(reports).

% he(1,1), refused(2,2), to(3,3),confirm(3, 4), or(4,5), deny(5,6),
% reports(6,7).

he(X,Y) <=> v(X,Y, np^he, he).

refused(X,Y) <=> v(X,Y, (s^refuse\np^I)/(s^J\np^I)^J, refused),
  c(refuse, _, I), c(refuse, _, J).

%to(X,Y) <=> v(X,Y, s^Z/s^Z, to), c(to, _, Z).
to(X,Y) <=> v(X,Y, S^Z/S^Z, to), c(to, _, Z).

confirm(X,Y) <=> v(X,Y, (s^confirm \ np^U)/NP^U, confirm),
  c(confirm, _, U), c(confirm, _, V).

or(X,Y) <=> v(X,Y, (Z\Z)/Z, or).
deny(X,Y) <=> v(X, Y, (s^deny\np^U)/np^V, deny),
    c(deny, _, U), c(deny, _, V).

reports(X,Y) <=> v(X,Y, np^reports, reports).


