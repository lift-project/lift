.. _testing:

Testing
=======

There exists a large set of tests.
These are regularly run by a continous integration (CI) server as part of the software development process.

A few tests with long execution time are not run by default, but these tests are still run by the CI server for pull requests.

Every single commit into the ``master`` branch **must** pass **all** tests.

Running tests
-------------

The test suite can either be run from the command line or via the IDE.

* To run the default set of tests from the command line execute the following command from the Lift root directory::

    > sbt test

* To run the tests in the IntelliJ IDEA IDE, right click on the ``src/test`` folder and select ``Run 'All Tests'``.

More verbose output can be enabled by setting the ``LIFT_VERBOSE`` environment variable.

NB: if you encounter a ``lift.arithmetic.NotEvaluableException`` the compiler
will not print the stack trace which can be annoying for debugging. This is
because this exception implements ``ControlThrowable`` (which itself implements
``NoStackTrace``) for performance reasons. If you want to temporary enable the
stack trace, change ``ControlThrowable`` to ``Throwable`` in the definition of
the exception and also change ``val`` to ``def`` in the companion object otherwise
you will always get the same stack trace. Now you should have a more helpful
exception.


Tests with long execution time
------------------------------

Some tests for the rewriting system can take several minutes to run and are therefore disabled by default.
They can be included by setting the ``LIFT_LONG_TESTS`` environment variable and rerunning the tests using ``sbt test`` or the IDE.
Tests are marked as a long running one, by calling the ``opencl.executor.LongTestEnabled()`` function in the test method or the entire class.

Ignoring tests
--------------

Tests related to issues which are not yet resolved are marked with the ``@Ignore`` annotation.
This annotation should be used only as an exception.

Tests for particular architectures
----------------------------------

Some tests use hardware features only available on specific architectures or trigger bugs on some architectures.
The methods in ``org.junit.Assume`` are used to ignore these based on some condition.

For example:

* | The device might not be able to use double precision;
  | See for example test ``opencl.generator.TestMisc.testDouble``.

* | The test uses warp sizes specific to NVIDIA GPUs;
  | See for example test ``opencl.generator.TestReduce.NVIDIA_C``.

* | The compiler for some architectures generates wrong code;
  | See for example test ``opencl.generator.TestReduce.NVIDIA_B``.
