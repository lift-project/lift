### How do I get set up? ###

1. Install mercurial and the latest Java SDK
2. Cone the "apart" repository to a directory of you choice
2. Download & Install IDEA IntelliJ (http://www.jetbrains.com/idea/)
3. Launch IntelliJ. On the Welcome Screen select "Configure" and install the "Scala" Plugin form the Plugin Repository. After the installation restart IntelliJ.
4. On the IntelliJ Welcome Screen select "Open Project" and select the "build.sbt" file in the "apart" folder you just checked out.

To be able to execute computations in OpenCL you need to install the SkelCL library.
You find general instructions to build &install SkelCL [here](https://github.com/skelcl/skelcl/wiki).
Please clone the SkelCL repository from [bitbucket](https://bitbucket.org/skelcl/skelcl) and not from github, as you need to checkout the 'Executor' branch!

1. Clone the SkelCL repository: git clone ssh://hg@bitbucket.org/skelcl/skelcl
2. Switch to the 'Executor' branch: cd skelcl && git checkout Executor
3.Install dependencies with the ./installDependenciesUbuntu.sh script. Answer all questions with y
4. Create a build directory, execute cmake and perform the build with make:
```
#!bash

mkdir build
cd build
cmake ..
make
```

Execute all tests with 
```
#!bash

make test
```
