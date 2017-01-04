Patterns
========

Algorithmic
-----------

.. lift.pattern:: Map : (a -> b) -> [a]_n -> [b]_n

The ``map`` pattern

.. lift.pattern:: Zip : [a]_n -> [b]_n -> [(a, b)]_n

The ``zip`` pattern

.. lift.pattern:: Reduce : ((a, a) -> a) -> a -> [a]_n -> [a]_1

The ``reduce`` pattern 

.. lift.pattern:: ReduceSeq : ((a, b) -> b) -> b -> [a]_n -> [b]_1

The ``reduceSeq`` pattern

.. lift.pattern:: Split : n -> [a]_{m} -> [[a]_{n}]_{m/n}

The ``split`` pattern

.. lift.pattern:: Join : [[a]_n]_m -> [a]_{n*m}

The ``join`` pattern

.. lift.pattern:: Iterate : n -> (m -> [a]_{k*m} -> [a]_m) -> [a]_{k^n*l} -> [a]_l

The ``iterate`` pattern

.. lift.pattern:: Reorder : [a]_n -> [a]_n

The ``reorder`` pattern

