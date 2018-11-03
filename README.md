# GlueCHR
# A quick and dirty implementation of Glue in Constraint Handling Rules (CHR).

Glue offers a simple, flexible conceptual framework for compositionally assembling meanings of natural language utterances, given an identification of appropriate syntactic roles in the utterance. It was originally developed in the context of LFG, but it is applicable to other settings as well, including CCG. It offers a clean formulation of semantic assembly while making relatively minimal requirements on syntactic theory. See [1] for more details.

The chief computational insight in Glue Semantics was the idea that one could assemble the meaning of an utterance from meanings of constituent phrases compositionally by using (forward chaining in) linear logic as the "glue" putting the pieces together. Linear logic permits formulas to be treated as resources, so they can be consumed during a derivation. This is crucial in accounting for the resource-conscious properties of language (whereby sentences such as `loves Hillary` are considered erroneous because they are missing a critical component, and `Bill loves Hillary Hillary` are considered erroneous because they have excess material). 

Glue can be thought of as using a restricted form of linear concurrent constraint (LCC) programming [3]. What is interesting is that a deep version is needed. To represent natural language quantifiers (using the generalized quantifier approach), glue uses nested computations. Specifically, a quantifier such as `everyone` is represented as
 ```
   (I)   forall H, S. (forall x. h ~> x -o H ~> S(x)) -o H ~> every(person, S)
```
Operationally, an arbitrary meaning (x) for `h` is postulated (via `h ~> x`), and from this the meaning as calculated at some (non-deterministic) scope `H`. From this meaning the functional dependence on `X` is extracted; `S` represents this meaning. If all this is possible then the meaning `every(person, S)` is asserted at the scope `H`. Properties of this form of nested forward chaining computation were developed (in the context of intuitionistic logic) in [4].

CHR is a simple implementation of a subset of flat LCC [2]. One can write out rules for meaning assembly directly in it, subject to these restrictions:
  1.  CHR does not have lambda-unification built in. However, we need only a restricted form of lambda-unification, one in which equations of the form `Y=S(X)` are to be solved, where `X` is a variable, `Y` is a term, and `S` is the abstraction to be discovered. Further, we are only interested in those cases where `Y` contains at least one occurrence of `X`, but is not `X`(the abstraction `S` is not vacuous). This is easy to implement programmatically. (See `lambda/3`.)

  2. CHR is flat. Therefore an alternate version of representation of nested quantifiers has to be used:
  ```
   (I') forall x. h ~> x ox (forall H, S. H ~> S(x) -o H ~> every(person, S))
 ```
 with the requirement that S be a non-vacuous generalization in x, i.e. `S(x)` must have at least one occurrence of `x`, and must not be `x`. This ensures that the resource `h ~> x` must be consumed in the proof of `H ~> S(x)`. Note that (I') implies (I) but is not implied by it.

  3. CHR is committed choice. This introduces indeterminism in the execution process. The order in which atoms are added to the store (the order of atoms in the formula) can affect the outcome. On a single run only one of the possible meanings (assuming there is at least one) will be generated. It is possible, in principle to generate all possible meanings by reordering the atoms in the formula.

The advantage of this implementation is that CHR is reasonably robust and mature, and is available integrated in many Prolog implementations that are in production use. Therefore this implementation of Glue semantics can be used directly. 

## EXAMPLES
 
The key insight in glue semantics is that one can separate out syntax from meaning assembly. The lexical entries come equipped with formulas in glue that specify how meanings are to be assembled, as a function of meanings associated with relevant "roles". 

Given a lexicon:
```
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
```
a sentence such as "Bill loves hillary" can be translated into the query:
```
?- v(bill, g), v(loves, g, h, f), v(hillary, h).
```
This query asserts that bill is associated with the g role (in a feature structure), hillary with the h role, and
loves with the g, h and f roles. Now the query yields:
```
?- v(bill, g), v(loves, g, h, f), v(hillary, h).
f~>loves(bill,hillary)
```
That is the meaning of the sentence (f) is the logical form 'loves(bill, hillary)'.

Other examples:
```
?-  v(bill, g), v(loves, g, h, f), v(hillary, h), v(obviously, f).
f~>obviously(loves(bill,hillary))
```
```
?- v(every, r, g), v(man, r), v(loves, g, h, f), v(a, r1, h), v(woman, r1).
 f~>a(woman,_G19008^every(man,_G19026^loves(_G19026,_G19008)))
true
```

By varying the order the other reading is obtained.
```
?- v(woman, r1), v(a, r1, h), v(loves, g, h, f), v(every, r, g), v(man, r).
f~>every(man,_G19860^a(woman,_G19878^loves(_G19860,_G19878)))
```

## References

[1] [Wikipedia page, Glue semantics.](https://en.wikipedia.org/wiki/Glue_semantics)

[2] [CHR](https://dtai.cs.kuleuven.be/CHR/)

[3] [Saraswat "LFG qua constraint programming"](http://cognet.mit.edu/book/semantics-and-syntax-lexical-functional-grammar)

[4] [Jagadeesan, Nadathur, Saraswat "Testing concurrent systems: An interpretation of intuitionistic logic", FSTTCS 2005]( 
http://saraswat.org/lambdarcc.pdf)
