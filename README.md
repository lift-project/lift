### The _Lift_ language ###

### Setup ###

1. Install git and the latest Java SDK
2. Clone the "lift" repository to a directory of you choice
2. Download & Install IDEA IntelliJ (http://www.jetbrains.com/idea/)
3. Launch IntelliJ. On the Welcome Screen select "Configure" and install the "Scala" Plugin form the Plugin Repository. After the installation restart IntelliJ.
4. On the IntelliJ Welcome Screen select "Open Project" and select the "build.sbt" file in the "lift" folder you just checked out.

To be able to execute computations in OpenCL you need to build the Executor library.
The script `executor.sh` builds the executor.
```
./executor.sh
```
The script is also called by sbt when running `sbt compile`

Finally add the Executor build directory to the Java library path:

* In IntelliJ select "Run -> Edit Configurations ..."

* On the left side select "Defaults -> JUnit"

* On the right side add to the "VM options": "-Djava.library.path=$LIFT_ROOT/lib/Executor/build/" (with the proper path from your system)

Alternatively, you can add the Executor build directory to your LD_LIBRARY_PATH. That will also make it possible to run tests and benchmarks from the command line using "sbt test" and scripts in the "scripts" folder, respectively.

### Documentation ###

[ScalaDoc](http://skelter:8080/job/lift-amd-cpu/branch/master/javadoc/#package)
