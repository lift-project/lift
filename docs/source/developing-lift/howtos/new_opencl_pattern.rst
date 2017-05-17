… add a new OpenCL pattern
--------------------------

Define an AST Node
^^^^^^^^^^^^^^^^^^

Each pattern is a case class extending ``ir.ast.Pattern``. It has to be
defined in the ``opencl.ir.pattern`` package. Take a look at the
``FPattern`` and ``isGenerable`` traits because you may want to implement
them.

When extending ``Pattern`` you should also provide the number of arguments your pattern
expects.

The ``FPattern`` trait is used when pattern matching to catch all patterns that contain
a ``Lambda`` inside them.

After defining you pattern, the first thing you have to do is override the ``checkType``
method which:

1. Checks that type of your pattern's arguments match its expectations.
2. Checks the types of any nested functions based on the pattern's input type.
3. Returns the type of your pattern's output,
   determined by the types its inputs or the nested functions.

The ``MapSeq`` pattern can be implemented like this:

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


Visiting the Pattern
^^^^^^^^^^^^^^^^^^^^

Some operations need to "visit" all nodes of an expression (which may contain
your pattern). You should take a look at the ``visit*`` and ``replace``
methods in ``ir/ast/Expr.scala`` to ensure you pattern is visited correctly.

Note: if you implement ``FPattern``, it may work out of the box. See how
``ReduceWhileSeq`` is handled as a typical example if you have to do
extra work.


Code Generation
^^^^^^^^^^^^^^^

Then you have to implement the code generation for your pattern. Look at
the ``OpenCLGenerator.generate`` method in the ``opencl.generator`` package.
It takes a type-checked lambda (e.g. an expression that can contain an instance
of your pattern) and returns a string (the OpenCL code).

Here are the different steps of this process.


OpenCL Address Spaces
"""""""""""""""""""""

OpenCL has a hierarchy of different address spaces: global memory, local memory and private memory.
You need to specify where to store the output of your pattern.
If you extend ``FPattern``, it might work out of the box without changing anything.
But you should take a look at ``InferOpenCLAddressSpace.setAddressSpaceFunCall``,
see how the other patterns are handled and probably add a case for your pattern.


Ranges and Counts
"""""""""""""""""

If your pattern generates a for loop in the OpenCL code, it should contain a
"loop variable" in its definition. The loop variable determines where the pattern
accesses its input array in every iteration of the generated loop.

Should your pattern contain a "loop variable", you should add a case in the
``RangesAndCounts.apply`` method. See how ``MapSeq`` is handled for example.


Memory Allocation
"""""""""""""""""

Only user functions allocate memory. You have to tell the nested user functions
how much memory they will need if they are nested in your pattern.

It is done in ``OpenCLMemoryAllocator.alloc``. More specifically, you
have to add a case in ``allocFunCall``.

All these functions take an argument ``inMem`` which is the memory, OpenCL
buffer(s), that the first user function in the pattern will read from.

Also, some other classes are called during this process. You may need to
edit ``OpenCLMemory`` as well.


Unrolling loops
"""""""""""""""

``ShouldUnroll`` isn't really an optimisation at this point. We have to unroll
loops when we use private memory as we flatten all private memory arrays into
variables and can't therefore index into them using variables. We represent the
private arrays as variables to try and force the compiler to store the arrays in
registers instead of spilling into global memory.  That means instead of a
definition ``int a[2];`` we have ``int a_0; int a_1; int a_2;`` and for reading/writing
``a[i]`` when ``i`` is ``1`` we need to emit ``a_1`` instead.

Barrier elimination
"""""""""""""""""""

``BarrierElimination`` is an optimisation consisting in removing unnecessary
barriers in the OpenCL generated code. Don't look at that in the first place.


Debugging
"""""""""

The ``if (Verbose()) { ... }`` part of the ``generate`` function prints
some helpful information, like types, that is displayed if you set the
``LIFT_VERBOSE`` environment variable. It will work out of the box if
you have correctly extended ``Expr``'s methods (see "Visiting your
pattern above)

The Views
"""""""""

The views are used to determine the locations in arrays where user functions in
any expression read from and write to.

Add support for your pattern in ``InputView.buildViewFunCall`` and
``OutputView.buildViewFunCall``. This means "explaining" how your pattern
modifies the reading/writing locations of nested functions.


Some Plumbing
"""""""""""""

The definition of tuple types and user functions should work out of the
box.


Generating the Kernel
"""""""""""""""""""""

Once all of the above passes have been implemented, you are able to
generate ``OpenCLAST`` nodes. This is done in ``generateKernel`` but
you probably do not have to edit this function and should directly look
at the private ``generate`` method of ``OpenCLGenerator``. Add a case
for your pattern.

It is probably a good idea to take a look at the classes defined in
``OpenCLAST.scala`` and at the utility functions like
``generateForLoop`` defined at the end of ``OpenCLGenerator.scala``.


Testing
^^^^^^^

You have to check that your pattern works as expected. For that add a
test class in the test folder in the ``opencl.generator`` package
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
^^^^^^^^^^^

-  Use the debugger to compare what you have in your pattern and in an
   already existing one at different points in the compilation process.
-  Look at the generated OpenCL code. To see it, enable the
   verbose output by setting the ``LIFT_VERBOSE``
   environment variable to ``1``.
-  Try to have something that compiles as soon as possible even if works
   only in some specific situations. It is easier to start from a
   simpler version of your pattern and then extend it.
