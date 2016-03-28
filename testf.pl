:- module(testf, [v/3,vv/3]).
:- use_module(library(chr)).
:-chr_constraint v/3, vv/3.

v(I,J,L), v(J,K,b(Z,L1)) <=> \+ (\+ L=L1) | L=L1, v(I,K,Z).
v(I,J,f(Z,R)), v(J,K,R1) <=> \+ (\+ R=R1) | R=R1, v(I,K,Z).

vv(I,J,L), vv(J,K,b(Z,L1)) <=> L=L1, vv(I,K,Z).
vv(I,J,f(Z,R)), vv(J,K,R1) <=> R=R1, vv(I,K,Z).

% ?- v(1,2, f(b(Z,Z),Z)), v(2,3,f(b(s,np),np)).
% ?- vv(1,2, f(b(Z,Z),Z)), vv(2,3,f(b(s,np),np)).