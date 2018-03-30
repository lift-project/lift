Building Lift
=============

After downloading the *Lift* repositroy and installing all dependencies (either directly or via docker) we will build the *Lift* compiler via ``sbt`` and to run all unit test to check if *Lift* is operating as expected.


Building the *Lift* Compiler
----------------------------
To build the *Lift* compiler simply run the following command from the repositroy root directory::

    > sbt compile

When executed the first time, this will:
  1. Bootstrap Scala by downloading the required Scala version;

  2. Download the ``arithexpr`` [#arithexpr]_ library used by the main *Lift* compiler;

  3. Compile the native library used by *Lift* to communicate with OpenCL;
  
  4. Compile the Scala and Java files implementing the *Lift* compiler.


Running all unit tests
----------------------
To run all unit tests simply run::

    > sbt test

This might take a few minutes and should run without any tests failing.

You can read more about the unit tests in the section about :ref:`testing`.


Configuring the IntelliJ IDEA IDE (optional)
--------------------------------------------
If you want to use an integrated development environment (which might be a good choice if you are new to *Lift* or maybe Scala altogether) we recommend `IntelliJ IDEA <https://www.jetbrains.com/idea/>`_ from Jetbrains.

There is a free *Community* edition of IDEA available to download from `Jetbrains website <https://www.jetbrains.com/idea/>`_.
It is important to install the Scala plugin, which is possible as part of the first launch setup of IDEA.

To configure the *Lift* project in the IDE perform the following steps:

  1. Launch IDEA and select ``"Import Project"`` from the launch screen:

    .. image:: images/IDEALaunchScreen.*
  
    Or select ``"File -> New -> Project from Existing Source ..."``:

    .. image:: images/IDEAMenu.*


  2. Select the ``build.sbt`` file at the root of the *Lift* repositroy:

    .. image:: images/IDEASBTSelection.*

  3. In the appearing ``"Import Project from SBT"`` dialog ensure that there is a *Project SDK* selected with at least *Java version* 1.8:

     .. image:: images/IDEAImportDialog.*

     If this is not the case click ``"New ..."`` next to *Project SDK* and select a Java SDK of version 1.8 or newer installed on your machine [#sdk]_.

  4. Click ``OK`` and also ``OK`` in the following dialog ``"SBT Project Data To Import"``:

     .. image:: images/IDEASBTModules.*

  6. Click ``"Run -> Edit Configurations"`` and then ``"Default -> JUnit"`` on the left pane of the dialog and add the following to ``"VM Options"`` on the right:
       ```-Djava.library.path=$LIFT_ROOT/lib/Executor/build/" (with the proper path from your system)```

     .. image:: images/IDEAVMOptions.*
  
    Alternatively, the ``Executor`` build directory can be added directly to your *LD_LIBRARY_PATH*. 

  6. Click ``"Build -> Build Project"`` to build the *Lift* compiler:

     .. image:: images/IDEABuild.*
  
     This is the equivalent action to ``sbt build`` on the command line.

  7. To run all unit tests unfold the project structure as shown in the picture:

     .. image:: images/IDEAProjectScreen.*

     Then right click on the ``test`` folder and select ``"Run 'All Tests'"``:

     .. image:: images/IDEARunAllTests.*

     This is the equivalent action to ``sbt test`` on the command line.

There is exhaustive documentation material for IDEA available at https://www.jetbrains.com/help/idea/2016.3/meet-intellij-idea.html, if you are unfamiliar with the IDE.



.. rubric:: Links

.. [#arithexpr] The ``arithexpr`` library is a Scala library developed by the *Lift* team and also hosted on github at https://github.com/lift-project/arithexpr.

.. [#sdk] You can read more about configuring SDKs in IDEA at https://www.jetbrains.com/help/idea/2016.3/sdk.html.
