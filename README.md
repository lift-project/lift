### Setup ###

1. Install mercurial and the latest Java SDK
2. Clone the "apart" repository to a directory of you choice
2. Download & Install IDEA IntelliJ (http://www.jetbrains.com/idea/)
3. Launch IntelliJ. On the Welcome Screen select "Configure" and install the "Scala" Plugin form the Plugin Repository. After the installation restart IntelliJ.
4. On the IntelliJ Welcome Screen select "Open Project" and select the "build.sbt" file in the "apart" folder you just checked out.

To be able to execute computations in OpenCL you need to install the SkelCL library, which is embedded as a submodule.

The script `skelcl.sh` builds SkelCL. The script is also called by sbt when running `sbt compile`
```
./skelcl.sh
```

You find more instructions to build &install SkelCL [here](https://github.com/skelcl/skelcl/wiki) if you want to do it manually.

Finally add the SkelCL build directory to the Java library path:

* In IntelliJ select "Run -> Edit Configurations ..."

* On the left side select "Defaults -> JUnit"

* On the right side add to the "VM options": "-Djava.library.path=/path-to-the-skelcl-folder/build/executor" (with the proper path from your system)

Alternatively, you can add the SkelCL build directory to your LD_LIBRARY_PATH. That will also make it possible to run tests and benchmarks from the command line using "sbt test" and scripts in the "scripts" folder, respectively.

### Documentation ###

[ScalaDoc](http://skelter:8080/job/apart-amd/branch/master/javadoc/#package)

### Build Status ###

Platform      | Status
------------- | -------------
Intel         | ![](http://skelter:8080/job/apart-intel/branch/master/badge/icon)
AMD           | ![](http://skelter:8080/job/apart-amd/branch/master/badge/icon)
AMD-GPU       | ![](http://skelter:8080/job/apart-amd-gpu/branch/master/badge/icon)
NVIDIA        | ![](http://skelter:8080/job/apart-nvidia/branch/master/badge/icon)