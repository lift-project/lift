Rewrite Rules
=============

Algorithmic Rewrite Rules
-------------------------

TODO: distinguish axioms from theorems

map(f) o map(g) == map(f o g)

reduceSeq(z, f) o mapSeq(g) == reduceSeq(z, (acc,x) -> f(acc, g(x)) )

reduce(z, f) == reduce(z, f) o partialReduce(z, f)

partialReduce(z, f) == join o map(partialReduce(z, f)) o split   | if f is associative and z is the neutral element (i.e. f(z,x) = x)

New rules (possibly unimplemented/unverified)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

reduce(z, map(f) o zip) == map( (e) -> (reduce(e._0, f) (e._1) )) o zip(z) o T

map(map(f) o T) o slide(step,size) == map(map(f) o slide(step,size)) o T



OpenCL Rewrite Rules
--------------------

New rules (possibly unimplemented/unverified)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

mapSeq(f) o slide(step,size) == slideSeqPlus(step,size,f)
