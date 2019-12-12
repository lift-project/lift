### The _Lift_ language ###

### Setup ###

1. Install `git`, `git-lfs`, `dot` and Oracle's Java SDK 8
    * OpenJDK or newer Java versions will not work. 
2. Clone the "lift" repository to a directory of your choice
    * Check out the branch of your choice
3. Install the `ArithExpr` dependency by running `./updateSubmodules.sh`
4. Download & Install IDEA IntelliJ (http://www.jetbrains.com/idea/)
5. Launch IntelliJ. On the Welcome Screen select "Configure" and install the "Scala" Plugin form the Plugin Repository. After the installation restart IntelliJ.
6. On the IntelliJ Welcome Screen select "Open Project" and select the "build.sbt" file in the "lift" folder you just checked out.
    * If you run into problems compiling files with Unicode names such as the lambda sign, change your system locale to UTF-8 by setting an environment variable in ~./bashrc

To be able to execute computations in OpenCL you need to build the Executor library.
The script `buildExecutor.sh` builds the executor.
```
./buildExecutor.sh
```
Both scripts are also called by sbt when running `sbt compile`

Finally add the Executor build directory to the Java library path:

* In IntelliJ select "Run -> Edit Configurations ..."

* On the left side select "Defaults -> JUnit"

* On the right side add to the "VM options": "-Djava.library.path=$LIFT_ROOT/lib/Executor/build/" (with the proper path from your system)

Alternatively, you can add the Executor build directory to your LD_LIBRARY_PATH. That will also make it possible to run tests and benchmarks from the command line using "sbt test" and scripts in the "scripts" folder, respectively.

### Documentation ###

[More Extensive Documentation on Building and Running Lift](http://lift-project.readthedocs.io/en/latest/)

[ScalaDoc](http://skelter:8080/job/lift-amd-cpu/branch/master/javadoc/#package)
