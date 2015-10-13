#pragma once

#include <functional>

// [local includes]
#include "opencl_utils.h"


// Object representation of the run
struct Run {

  // global range
  std::size_t glob1 = 0;
  std::size_t glob2 = 0;
  std::size_t glob3 = 0;

  // local range
  std::size_t loc1 = 0;
  std::size_t loc2 = 0;
  std::size_t loc3 = 0;

  // hash file
  std::string hash;

  // compiled kernel
  cl::Kernel kernel;

  Run(){}

  // Load the file and compile the program
  bool compile(cl::Context context,
               std::vector<cl::Device> dev,
               std::function<bool(cl::Kernel&,size_t,size_t,size_t)> compatibility,
               bool binary_mode);

  virtual ~Run(){}

  virtual void setup(cl::Context context) = 0;

  virtual void cleanup() = 0;

  cl::Kernel& getKernel() { return kernel; }

protected:
  // Get a file into a string
  std::string loadCode(bool binary);
};