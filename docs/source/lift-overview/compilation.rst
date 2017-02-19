Compilation Flow
================

Compiling a program from the *Lift* language to OpenCL has the following steps [#steuwer17LiftIR]_:

#. Type Analysis
#. Memory Allocation
#. Multi-Dimensional Array Accesses
#. Barrier Elimination
#. OpenCL Code Generation

.. [#steuwer17LiftIR] Lift: A Functional Data-Parallel IR for High-Performance GPU Code Generation: http://www.lift-project.org/papers/steuwer17LiftIR.pdf

Inferring OpenCL Thread Counts
==============================

To automatically determine how many threads to launch for a rewritten program, or as a convenience, the following simple algorithm is used.

The algorithm tries to eliminate as many loops resulting from the different ``Map`` patterns as possible. 
It'll choose thread counts to match the data sizes being mapped over. 
So, e.g. a ``MapGlb`` over an array of length ``N`` would get N threads in that dimension (and the local size would be undefined).
similarly the number of work-group for ``MapWrg`` and the number local threads for ``MapLcl`` are determined.
If there are several ``MapLcl`` in the same dimension, the most common thread count will be chosen, e.g. if there are 3 ``MapLcl`` in dimension 0, mapping over arrays of lengths 32, 16 and 32, 32 will be chosen. 
If there is no ``MapGlb`` or ``MapWrg`` and ``MapLcl`` pair in some dimension, a thread count of 1 will be chosen no parallelism is present.


