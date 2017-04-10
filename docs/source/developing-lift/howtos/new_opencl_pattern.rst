… add a new OpenCL pattern
--------------------------

Define an AST node
^^^^^^^^^^^^^^^^^^

Each pattern is a case class extending ``ir.ast.Pattern``. It has to be
defined into the ``opencl.ir.pattern`` package. Take a look at the
``FPattern`` and ``isGenerable`` traits because you may want to extend
them.

The first thing you have to do then is to override the ``checkType``
method which:

1. Checks the type of your pattern's arguments, nested functions, etc.
2. Returns the type of your pattern's output.

Basically, the ``MapSeq`` pattern could be implemented like that:

.. code:: scala

    /**
     * Sequential map
     *
     * @param f a function to be applied to every element of the input array
     */
    case class MapSeq(f: Lambda1) extends Pattern(arity=1)
                                  with FPattern
                                  with isGenerable {

      override def checkType(argType: Type, ...): Type =
        argType match {
          case ArrayType(ty, len) => {
            // f has type: `argType -> a`
            f.params.head.t = argType
            // map(f) has type `[argType],,n,, -> [a],,n,,`
            ArrayType(TypeChecker.check(f), len)
          }
          case _ => // throw an exception
        }
    }


Visiting your pattern
^^^^^^^^^^^^^^^^^^^^^

Some operations need to "visit" a whole expression (which may contain
your pattern). You should take a look at the ``visit*`` and ``replace``
methods in ``Expr.scala`` to ensure you pattern is visited correctly.

Note: if you extend ``FPattern``, it may work out of the box. See how
``ReduceWhileSeq`` is handled as a typical example where you have to do
extra work.


The code generation
^^^^^^^^^^^^^^^^^^^

Then you have to implement the code generation for your pattern. Look at
the public version of ``OpenCLGenerator.generate`` in the package
``opencl.generator``, it takes a type-checked lambda (e.g. an instance
of your pattern) and returns OpenCL code.

Here are the different steps of this process.


OpenCL address space
""""""""""""""""""""

OpenCL has different address spaces, you need to specify where to store
what for your pattern. If you extend ``FPattern``, it might work out of
the box without changing anything… But you should take a look at
``InferOpenCLAddressSpace.setAddressSpaceFunCall``, see how the other
patterns are handled and probably add a case for your pattern.


Ranges and counts
"""""""""""""""""

If your pattern contains in its definition a "loop variable" - basically
the index used to traverse an array - you should add a case in
``RangesAndCounts``'s apply method. See how ``MapSeq`` is handled for
example.


Memory allocation
"""""""""""""""""

You have to allocate memory for your pattern:

1. The eventual nested functions it contains
2. It's output

It is done in ``OpenCLMemoryAllocator.alloc``. More specifically, you
have to add a case in ``allocFunCall``.

**TODO**: I don't understand, what's happening with the input? What is
``inMem``?

Also, some other classes are called during this process. You may need to
edit ``OpenCLMemory.scala`` as well.


Optimizations
"""""""""""""

``ShouldUnroll`` and ``BarrierElimination`` are optimizations you might
be able to perform at some point. Don't look at that in the first place.


Debugging stuff
"""""""""""""""

The ``if (Verbose()) { ... }`` part of the ``generate`` function prints
some helpful information that are displayed if you set the
``LIFT_VERBOSE`` environment variable. It will work out of the box if
you have correctly extended ``Expr``'s methods (see "Visiting your
pattern above)

The views
"""""""""

The views are an abstraction used to represent the input and the output
of any expression. **TODO:** is that right?

Add support for your pattern in ``InputView.buildViewFunCall`` and
``OutputView.buildViewFunCall``


Some plumbing
"""""""""""""

The definition of tuple types and user functions should work out of the
box.


Actually generate the kernel
""""""""""""""""""""""""""""

Once all of the above passes have been performed, your are able to
generate ``OpenCLAST`` nodes. This is done in by ``generateKernel`` but
you probably do not have to edit this function and should directly look
at the private ``generate`` method of ``OpenCLGenerator``. Add a case
for your pattern.

It is probably a good idea to take a look at the classes defined in
``OpenCLAST.scala`` and at the utility functions like
``generateForLoop`` defined at the end of ``OpenCLGenerator.scala``.


Finally test it
---------------

You have to check that your pattern works as expected. For that add a
test class under the test folder in the ``opencl.generator`` package
with some tests.

For example, for ``MapSeq``, you could have:

.. code:: scala

    object TestMapSeq {
      @BeforeClass def before(): Unit = {
        Executor.loadLibrary()
        println("Initialize the executor")
        Executor.init()
      }
      
      @AfterClass def after(): Unit = {
        println("Shutdown the executor")
        Executor.shutdown()
      }
    }

    class TestMapSeq {
      @Test def simpleMap(): Unit = {
        val size = 1024
        val input = Array.fill(size)(util.Random.nextInt)
        val N = SizeVar("N")

        val add2 = UserFun("add2", "x", "return x+2;", Int, Int)

        val kernel = fun(
          ArrayType(Int, N),
          array => MapSeq(add2) $ array
        )

        val (output: Array[Int], _) = Execute(size)(kernel, input)

        assertArrayEquals(input.map(_ + 2), output)
      }
    }


Useful tips
-----------

-  Use the debugger to compare what your have on your pattern and on an
   already existing one at different points in the compilation process.
-  Look at the generated OpenCL code. In that purpose, enable the
   verbose output for the executor by setting the ``LIFT_VERBOSE``
   environment variable to ``1``.
-  Try to have something that compiles as soon as possible even if works
   only in some specific situations. It is easier to start from a
   simpler version of your pattern and then extend it.
