///
/// \file Kernel.h
///
/// \author Michel Steuwer <michel.steuwer@ed.ac.uk>
///

#ifndef KERNEL_H_
#define KERNEL_H_

#include <string>

#define __CL_ENABLE_EXCEPTIONS
#include <CL/cl.hpp>
#undef  __CL_ENABLE_EXCEPTIONS

#include "JNIHandle.h"

namespace executor {

class Kernel: public JNIHandle {
public:
  static Kernel* create(std::string kernelSource, std::string kernelName, std::string buildOptions);

  Kernel(std::string kernelSource, std::string kernelName, std::string buildOptions);

  cl::Kernel build() const;
  std::string getSource() const;
  std::string getName() const;
  std::string getBuildOptions() const;

private:
  std::string kernelSource;
  std::string kernelName;
  std::string buildOptions;
  mutable cl::Kernel kernel;
};

}

#endif // KERNEL_H_