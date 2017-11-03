High-Level Rewrite
==================

This stage performs algorithmic rewriting of the input program.

Filtering Heuristics
--------------------

Expression Nesting Depth
^^^^^^^^^^^^^^^^^^^^^^^^

Counts how deeply `Map`/`Reduce` patterns are nested inside the program.
The deepest nesting is reported.

Some examples::

  \(input => plusOne $ input) // Depth 0

  \(input => Map(plusOne) $ input) // Depth 1
  \(input => Map(plusOne) o Map(plusOne) $ input) // Depth 1
  \(input => Reduce(add, 0.0f) $ input) // Depth 1

  \(input => Map(Map(plusOne)) $ input) // Depth 2

  \(input => Map(Map(Map(plusOne))) $ input) // Depth 3
  \(input => Map(Map(plusOne) o Join() o Map(Map(plusOne))) $ input) // Depth 3
  \(input => Map(Map(Reduce(add, 0.0f))) $ input) // Depth 3

Adjusted using the ``--depth`` command line option.

User-Function Distance
^^^^^^^^^^^^^^^^^^^^^^

Tries to evaluate how well the rewritten program has simplified by counting the number of data-layout patterns on the data-flow path between user-functions.
Simplification here refers to removing superfluous data-layout patterns (`Split`, `Join`, `Scatter`, `Gather`, `Transpose`, `TransposeW`, `asVector` and `asScalar`).
The largest count is reported.

Some examples::

  \(input => Map(plusOne) $ input) // Distance 0, only one user-function

  \(input => Map(add) $ Zip(Map(plusOne) $ input, Map(plusOne) $ input)) // Distance 0

  \(input => Map(plusOne) o Join() o Map(Map(plusOne)) $ input) // Distance 1, Join

  \((input2D, input1D) => 
    Map(add) $ Zip(Join() o Map(Map(plusOne)) $ input2D, Map(plusOne) $ input1D)) // Distance 1

  \(input => Map(Map(plusOne)) o Split(x) o Join() o Map(Map(plusOne)) $ input) // Distance 2, Split and Join

Adjusted using the ``--distance`` command line option.
