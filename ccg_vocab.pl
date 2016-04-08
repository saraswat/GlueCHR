%:-module(ccg,[e/3]).
				% entries in the lexicon
:- op(400, xfy, user:(\)).
e(refused, (s\np)/(s\np), P^X^E^and([refuse*E, a0*X*E, exists(E1^and([P*X*E1,a1*E1*E]))])).
e(to,      (s\np)/(s\np), P^X^E^(P*X*E)).
e(or,      (Z\Z)/Z,       Q^P^X^Y^E^or([P*X*Y*E,Q*X*Y*E])).
e(V,       (s\np)/np,     X^Y^E^and([V*E, ao*Y*E, a1*X*E])) :- trans_verb(V). 
e(NP,       np,            NP) :- np(NP).				
e(if,      (s/s)/s,       P^Q^E^(implies*(P*E)*exists(E1^(Q*E1)))).
e(then,    (s\np)\(s\np), P^X^E^and([P*X*E, then*E])).
e(Adj,     (s\np),     X^E^and([Adj*E,arg*X*E])) :- adj(Adj).
e(are,     (s\np)/(s\np), P^X^E^(P*X*E)).

trans_verb(loves).
trans_verb(confirm).
trans_verb(deny).

np(he).
np(ed).
np(ann).
np(bill).
np(hillary).
np(reports).
np(roses).
np(violets).

adj(red).
adj(blue).
