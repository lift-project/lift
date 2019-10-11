Installation
============

There are two main options how to install *Lift*:

* Maybe the easiest options is to install it via ``docker`` but unfortunately, accessing GPUs from ``docker`` might be an issue.

* For proper development we recommend to install *Lift* natively which provides the most flexibility but might require a bit more time to setup.

.. note:: *Lift* currently is only supported on Linux machines

Installation via Docker
-----------------------
`Docker <https://www.docker.com/>`_ is a technology which allows to package software with all its dependencies into a *container*.

We have bundled *Lift* an all of its dependencies inside such a container.
The *Lift* container can be executed either directly via ``docker``, which does not allow the execution of *Lift* programs on GPUs,
or via ``nvidia-docker``, which allows the execution of *Lift* programs on Nvidia GPUs from inside the docker container.

Option A: Using plain ``docker``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
To create and use the *Lift* docker container follow these steps:

1. Install ``docker``

   Docker requires a 64-bit Linux installation with a kernel version 3.10 or higher.
   Instructions to install docker can be found here: https://docs.docker.com/engine/installation/.

2. Build the *Lift* docker container image

   To build the container image and giving it the name ``lift`` execute the following command from the *Lift* root directory.
   ::

       docker build -t lift docker/lift/dev

3. Run the *Lift* docker container

   To run the container in with an interactive shell execute the following command from the *Lift* root directory.
   ::

       docker run -it --rm -v `pwd`:/lift lift

   This command makes the current directory accessible inside the container under the path ``/lift``.
   The container will be deleted after you exit the interactive shell.
   Changes to the files in the ``/lift`` directory of the container are persistent while all other changes inside the container are not.

4. Compile *Lift* and run the test suite

    You are now ready to compile *Lift* and run the test suite to see if everything works as expected.

Option B: Using ``nvidia-docker``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
To create and use the *Lift* docker container with ``nvidia-docker`` follow these steps:

1. Install ``docker`` **and** ``nvidia-docker``

   Docker requires a 64-bit Linux installation with a kernel version 3.10 or higher.
   Instructions to install docker can be found here: https://docs.docker.com/engine/installation/.

   ``nvidia-docker`` is a plugin for docker which makes Nvidia GPUs accessible inside the container.
   Installation instructions for ``nvidia-docker`` can be found here: https://github.com/NVIDIA/nvidia-docker/wiki/Installation.

2. Build the *Lift* docker container image

   To build the container image and giving it the name ``lift`` execute the following command from the *Lift* root directory.
   ::

       docker build -t lift docker/lift/dev

3. Run the *Lift* docker container

   To run the container in with an interactive shell execute the following command from the *Lift* root directory.
   ::

       nvidia-docker run -it --rm -v `pwd`:/lift lift

   This command makes the current directory accessible inside the container under the path ``/lift``.
   The container will be deleted after you exit the interactive shell.
   Changes to the files in the ``/lift`` directory of the container are persistent while all other changes inside the container are not.

4. Compile *Lift* and run the test suite

    You are now ready to compile *Lift* and run the test suite to see if everything works as expected.


Native Installation
-------------------
*Lift* has a number of dependencies which have to be installed first before compiling and using *Lift*.

Dependencies
^^^^^^^^^^^^
*Lift* has the following dependencies:

* OpenCL
* CMake and a C++ compiler (i.e. ``gcc`` or ``clang``)
* Java 8 SDK
* ``SBT`` (the *Scala Build Tool*)

In the following we individually discuss how to install the dependencies.


OpenCL
""""""
OpenCL is a framework for programming heterogeneous devices such as GPUs.
*Lift* currently generates OpenCL kernels, functions implemented in the OpenCL C programming language and executed in parallel.
Therefore, you have to have a working OpenCL implementation for executing *Lift* programs.

`Andreas Klöckner <https://andreask.cs.illinois.edu/aboutme>`_ maintains a very good guide on how to install OpenCL for Linux: https://wiki.tiker.net/OpenCLHowTo.


CMake and a C++ Compiler
""""""""""""""""""""""""
For *Lift* to communicate with OpenCL it relies on a small library which connects the *Lift* compiler implemented in Scala and running in the Java virtual machine to the C OpenCL implementation.
To compile this library we need a C++ compiler and CMake.

A C++ Compiler should be easy to install via your Linux distribution's package manager.

Similarly, CMake should be easy to install via your package manage.
If not, CMake is easy to build from source as explained here: https://cmake.org/install/.


Java
""""
*Lift* is implemented in Scala which is a programming language running on top of the Java virtual machine.
Therefore, a Java installation is required for running *Lift*.
The package manager of your Linux distribution will most likely provide a Java implementation.
Oracle Java implementation is accessible here: http://www.oracle.com/technetwork/java/javase/downloads/index.html.

.. note:: Java 8 is required for *Lift*. This is a strict requirement!

          To check the installed Java version run::

              > java -version

          and
          ::

              > javac - version

          Both commands should print ``"1.8.0"`` or newer.
          If this is not the case you have to install a newer version of Java.


``SBT``
"""""""
``SBT``, the *Scala Build Tool*, is a versatile tool for building Scala source code.
It downloads the exact version of the Scala compiler required by *Lift*.
It also handles all dependencies to Scala libraries.

To install ``SBT`` follow the instructions provided here: http://www.scala-sbt.org/0.13/docs/Installing-sbt-on-Linux.html.













