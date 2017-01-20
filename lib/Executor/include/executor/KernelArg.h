///
/// \file KernelArg.h
///
/// \author Michel Steuwer <michel.steuwer@ed.ac.uk>
///


#ifndef KERNEL_ARG_H_
#define KERNEL_ARG_H_

#include "Core.h"
#include "JNIHandle.h"

namespace executor {

class KernelArg : public JNIHandle {
public:
  virtual void setAsKernelArg(cl::Kernel kernel, int i) = 0;
  virtual void upload() = 0;
  virtual void download() = 0;
  virtual void clear();
};

}

#endif // KERNEL_ARG_H_