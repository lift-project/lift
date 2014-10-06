### How do I get set up? ###

1. Install mercurial and the latest Java SDK
2. Cone the "apart" repository to a directory of you choice
2. Download & Install IDEA IntelliJ (http://www.jetbrains.com/idea/)
3. Launch IntelliJ. On the Welcome Screen select "Configure" and install the "Scala" Plugin form the Plugin Repository. After the installation restart IntelliJ.
4. On the IntelliJ Welcome Screen select "Open Project" and select the "build.sbt" file in the "apart" folder you just checked out.

To be able to execute computations in OpenCL you need to install the SkelCL library.
Please clone the SkelCL repository from [bitbucket](https://bitbucket.org/skelcl/skelcl) and not from github, as you need to checkout the 'Executor' branch!

*  Clone the SkelCL repository: 
```
#!bash

git clone git@bitbucket.org:skelcl/skelcl.git
```
* Switch to the 'Executor' branch: 
```
#!bash

cd skelcl && git checkout Executor
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