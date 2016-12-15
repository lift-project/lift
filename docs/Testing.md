# Testing

## Standard tests

The test suite can be run from the command line or the IDE.
To run the entire suite from the command line use `sbt test`.
To do the same in IntelliJ IDEA, right click the `src/test` folder and select `Run 'All Tests'`.
More verbose output can be enabled by setting the `LIFT_VERBOSE` environment variable.

## Longer tests

Some tests for the rewriting system can take several minutes each to run and are therefore disabled by default.
They can be included by setting the `LIFT_LONG_TESTS` environment variable and rerunning the tests.
Tests can be marked as a long ones, by calling `opencl.executor.LongTestEnabled()` in the test method or class.

## Ignoring tests

Tests related to issues not resolved yet are marked with the `@Ignore` annotation.

## Tests for particular architectures

Some tests use hardware features only available on specific architectures or trigger bugs on some architectures.
The methods in `org.junit.Assume` are used to ignore these based on some condition.

For example:
* The device might not be able to use double precision (e.g. `opencl.generator.TestMisc.testDouble`).
* The test uses warp sizes specific to NVIDIA GPUs (e.g. `opencl.generator.TestReduce.NVIDIA_C`).
* The compiler for some architectures generates wrong code (e.g. `opencl.generator.TestReduce.NVIDIA_B`).
