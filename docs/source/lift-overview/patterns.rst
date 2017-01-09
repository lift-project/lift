Patterns
========

Algorithmic Patterns
--------------------

.. lift.pattern:: Map : (a -> b) -> [a]_n -> [b]_n

  The ``map`` pattern

.. lift.pattern:: Zip : [a]_n -> [b]_n -> [(a, b)]_n

  The ``zip`` pattern

.. lift.pattern:: Reduce : ((a, a) -> a) -> a -> [a]_n -> [a]_1

  The ``reduce`` pattern 

.. lift.pattern:: ReduceSeq : ((a, b) -> a) -> a -> [b]_n -> [a]_1

  The ``reduceSeq`` pattern

.. lift.pattern:: Split : n -> [a]_{m} -> [[a]_{n}]_{m/n}

  The ``split`` pattern

.. lift.pattern:: Join : [[a]_n]_m -> [a]_{n*m}

  The ``join`` pattern

.. lift.pattern:: Iterate : n -> (m -> [a]_{k*m} -> [a]_m) -> [a]_{k^n*l} -> [a]_l

  The ``iterate`` pattern

.. lift.pattern:: Reorder : ((Int, Type) -> Int) -> [a]_n -> [a]_n

  The ``reorder`` pattern

.. lift.pattern:: Transpose : [[a]_m]_n -> [[a]_n]_m

  The ``Transpose`` pattern. Added for convenience, can be implemented using ``Join``, ``Reorder`` and ``Split``.

OpenCL Patterns
---------------

.. lift.pattern:: MapGlb : (a -> b) -> [a]_n -> [b]_n

  The ``map`` pattern for mapping work onto global threads.

.. lift.pattern:: MapWrg : (a -> b) -> [a]_n -> [b]_n

  The ``map`` pattern for mapping work onto work-groups (groups of threads).

.. lift.pattern:: MapLcl : (a -> b) -> [a]_n -> [b]_n

  The ``map`` pattern for mapping work onto local threads.

.. lift.pattern:: MapSeq : (a -> b) -> [a]_n -> [b]_n

  The ``map`` pattern for mapping work sequentially.

.. lift.pattern:: PartRed : ((a,a) -> a) -> a -> m -> [a]_{m*n} -> [a]_m

  The ``PartRed`` pattern. Performs a partial reduction to size ``m``.

.. lift.pattern:: toGlobal : (a -> b) -> (a -> b)

  The ``toGlobal`` pattern

.. lift.pattern:: toLocal : (a -> b) -> (a -> b)

  The ``toLocal`` pattern

.. lift.pattern:: toPrivate : (a -> b) -> (a -> b)

  The ``toPrivate`` pattern

.. lift.pattern:: asVector : m -> [a]_{m*n} -> [<a>_n]_m

  The ``asVector`` pattern.

.. lift.pattern:: asScalar : [<a>_m]_n -> [a]_{m*n}

  The ``asScalar`` pattern.

.. lift.pattern:: Vectorize : m -> (a -> b) -> (<a>_m -> <b>_m)

  The ``Vectorize`` pattern.

