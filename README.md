### Setup ###

1. Install mercurial and the latest Java SDK
2. Cone the "apart" repository to a directory of you choice
2. Download & Install IDEA IntelliJ (http://www.jetbrains.com/idea/)
3. Launch IntelliJ. On the Welcome Screen select "Configure" and install the "Scala" Plugin form the Plugin Repository. After the installation restart IntelliJ.
4. On the IntelliJ Welcome Screen select "Open Project" and select the "build.sbt" file in the "apart" folder you just checked out.

To be able to execute computations in OpenCL you need to install the SkelCL library.
Please clone the SkelCL repository from [bitbucket](https://bitbucket.org/skelcl/skelcl) or [github](https://github.com/skelcl/skelcl).

*  Clone the SkelCL repository: 
```
#!bash

git clone git@bitbucket.org:skelcl/skelcl.git
```
* Install dependencies with the ./installDependenciesUbuntu.sh script. Answer all questions with 'y'
* Create a build directory, execute cmake and perform the build with make:
```
#!bash

mkdir build
cd build
cmake ..
make
```
* Execute all tests with 
```
#!bash

make test
```

You find more instructions to build &install SkelCL [here](https://github.com/skelcl/skelcl/wiki).

Finally add the SkelCL build directory to the Java library path:

* In IntelliJ select "Run -> Edit Configurations ..."

* On the left side select "Defaults -> JUnit"

* On the right side add to the "VM options": "-Djava.library.path=/path-to-the-skelcl-folder/build/executor" (with the proper path from your system)

Alternatively, you can add the SkelCL build directory to your LD_LIBRARY_PATH. That will also make it possible to run tests and benchmarks from the command line using "sbt test" and scripts in the "scripts" folder, respectively.