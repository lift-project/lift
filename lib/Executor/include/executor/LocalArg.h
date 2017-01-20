///
/// \file LocalArg.h
///
/// \author Michel Steuwer <michel.steuwer@ed.ac.uk>
///


#ifndef LOCAL_ARG_H_
#define LOCAL_ARG_H_

#include "Core.h"
#include "KernelArg.h"

namespace executor {

class LocalArg : public KernelArg {
public:
  static KernelArg* create(size_t sizeInBytes);

  void setAsKernelArg(cl::Kernel kernel, int i);
  void upload();
  void download();

private:
  LocalArg(size_t sizeP);

  size_t size;
};

}

#endif // LOCAL_ARG_H_