Patterns
========

Algorithmic Patterns
--------------------

.. lift.pattern:: Map : (a -> b) -> [a]_n -> [b]_n = map f [x_1, ..., x_n] = [f x_1, ..., f x_n]

  The ``map`` pattern

.. lift.pattern:: Zip : [a]_n -> [b]_n -> [(a, b)]_n = zip [x_1, ..., x_n] [y_1, ..., y_n] = [(x_1, y_1), ..., (x_n, y_n)]

  The ``zip`` pattern

.. lift.pattern:: Reduce : ((a, a) -> a) -> a -> [a]_n -> [a]_1 = reduce (+) 0 [x_1, ..., x_n] = [0 + x_1 + ... + x_n]

  The ``reduce`` pattern

.. lift.pattern:: ReduceSeq : ((a, b) -> a) -> a -> [b]_n -> [a]_1 = reduceSeq (+) 0 [x_1, x_2, ..., x_n] = [(...((0 + x_1) + x_2) + ... + x_n)]

  The ``reduceSeq`` pattern

.. lift.pattern:: Split : n -> [a]_{m} -> [[a]_{n}]_{m/n} = split n [x_1, ..., x_m] = [[x_1, ..., x_n], ..., [x_{m-n}, x_m]]

  The ``split`` pattern

.. lift.pattern:: Join : [[a]_n]_m -> [a]_{n*m} = join [[x_1, ..., x_n], ..., [x_{(m*n)-n}, x_{n*m}]] = [x_1, ..., x_{n*m}]

  The ``join`` pattern

.. lift.pattern:: Iterate : n -> (m -> [a]_{k*m} -> [a]_m) -> [a]_{k^n*l} -> [a]_l = iterate n f xs = iterate (n-1) f (f xs); iterate 0 f xs = xs

  The ``iterate`` pattern

.. lift.pattern:: Reorder : ((Int, Type) -> Int) -> [a]_n -> [a]_n = reorder idx xs = ys

  The ``reorder`` pattern

.. lift.pattern:: Transpose : [[a]_m]_n -> [[a]_n]_m = transpose [[x_{1,1}, ..., x_{1,m}], ..., [x_{n,1}, ..., x_{n,m}]] = [[x_{1,1}, ..., x_{n,1}], ..., [x_{1,m}, ..., x_{n,m}]]

  The ``Transpose`` pattern. Added for convenience, can be implemented using ``Join``, ``Reorder`` and ``Split``.

OpenCL Patterns
---------------

.. lift.pattern:: MapGlb : (a -> b) -> [a]_n -> [b]_n = mapGlb = map

  The ``map`` pattern for mapping work onto global threads.

.. lift.pattern:: MapWrg : (a -> b) -> [a]_n -> [b]_n = mapWrg = map

  The ``map`` pattern for mapping work onto work-groups (groups of threads).

.. lift.pattern:: MapLcl : (a -> b) -> [a]_n -> [b]_n = mapLcl = map

  The ``map`` pattern for mapping work onto local threads.

.. lift.pattern:: MapSeq : (a -> b) -> [a]_n -> [b]_n = mapSeq = map

  The ``map`` pattern for mapping work sequentially.

.. lift.pattern:: PartRed : ((a,a) -> a) -> a -> m -> [a]_{m*n} -> [a]_m

  The ``PartRed`` pattern. Performs a partial reduction to size ``m``.

.. lift.pattern:: toGlobal : (a -> b) -> (a -> b)

  The ``toGlobal`` pattern

.. lift.pattern:: toLocal : (a -> b) -> (a -> b)

  The ``toLocal`` pattern

.. lift.pattern:: toPrivate : (a -> b) -> (a -> b)

  The ``toPrivate`` pattern

.. lift.pattern:: asVector : m -> [a]_{m*n} -> [<a>_m]_n

  The ``asVector`` pattern.

.. lift.pattern:: asScalar : [<a>_m]_n -> [a]_{m*n}

  The ``asScalar`` pattern.

.. lift.pattern:: Vectorize : m -> (a -> b) -> (<a>_m -> <b>_m)

  The ``Vectorize`` pattern.

