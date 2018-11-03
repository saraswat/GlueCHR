# Installation instructions

Download and install SWI-Prolog.

Then consult the glue file, and run your queries.
```
 bash-3.2$ swipl
Welcome to SWI-Prolog (Multi-threaded, 64 bits, Version 7.2.3)
Copyright (c) 1990-2015 University of Amsterdam, VU Amsterdam
SWI-Prolog comes with ABSOLUTELY NO WARRANTY. This is free software,
and you are welcome to redistribute it under certain conditions.
Please visit http://www.swi-prolog.org for details.

For help, use ?- help(Topic). or ?- apropos(Word).

?- [glue].
true.

?- v(every, r, g), v(man, r), v(loves, g, h, f), v(a, r1, h), v(woman, r1).
f~>a(woman,_G16457^every(man,_G16475^loves(_G16475,_G16457)))
true 
f~>a(woman,_G16457^every(man,_G16475^loves(_G16475,_G16457)))

?- 
```
