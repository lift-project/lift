#pragma once

#include <functional>

#define __CL_ENABLE_EXCEPTIONS
#include <CL/cl.hpp>


// Object representation of the run
class Run {
public:

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
  bool compile(bool binary_mode);

  virtual ~Run(){}

  virtual void setup(cl::Context context) = 0;

  virtual void cleanup() = 0;

  cl::Kernel& getKernel() { return kernel; }
};