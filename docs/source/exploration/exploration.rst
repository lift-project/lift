Generating Kernels
==================

After having set up and built *Lift* navigate to the root folder of the repository and create the wrapper scripts for running the compiled programs::

  scripts/buildRunScripts.py

You can now run all stages of the rewrite system for a program (in this example for matrix multiplication)::

  scripts/compiled_scripts/HighLevelRewrite highLevel/mmTransposedA
  scripts/compiled_scripts/MemoryMappingRewrite --gr10 mmTransposedA
  scripts/compiled_scripts/ParameterRewrite -f highLevel/mm.json mmTransposedA

The generated OpenCL code for the application is now in the ``mmTransposedACl`` folder along with ``exec_*.csv`` files saying what thread-counts and memory sizes to use for executing them.

The results from the intermediate stages can be seen in the ``mmTransposedA`` and ``mmTransposedALower`` folders.

Running Kernels
===============

To run them clone the harness from https://github.com/lift-project/harness, build it using ``cmake`` and add the harness programs to your ``PATH``::

  git clone https://github.com/lift-project/harness.git
  cd harness
  mkdir build && cd build
  cmake ..
  make
  export PATH=`pwd`:$PATH


To run all program variations change to the mmTransposedACl folder and use the following command, substituting the desired platform and device numbers.::

  for i in `seq 1 250`; do find . -mindepth 1 -type d -exec sh -c '(cd {} && timeout 5m harness_mm -k 1024 -n 1024 -m 1024 --transpose-A -d $DEVICE -p $PLATFORM)' ';'; done

``-k 1024 -n 1024 -m 1024`` indicate the sizes to use for the inputs and can also be adjusted.

The runtimes for the kernels will be stored in ``time_*.csv`` files.
