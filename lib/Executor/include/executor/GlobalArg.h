///
/// \file GlobalArg.h
///
/// \author Michel Steuwer <michel.steuwer@ed.ac.uk>
///


#ifndef GLOBAL_ARG_H_
#define GLOBAL_ARG_H_

#include <vector>

#include "Core.h"
#include "KernelArg.h"
#include "Vector.h"

namespace executor {

class GlobalArg : public KernelArg {
public:
  static KernelArg* create(void* data, size_t sizeInBytes,
                           bool isOutput = false);
  static KernelArg* create(size_t sizeInBytes, bool isOutput = false);

  void setAsKernelArg(cl::Kernel kernel, int i);
  void upload();
  void download();

  const executor::Vector<char>& data() const;

  void clear();
  
private:
  GlobalArg(executor::Vector<char>&& vectorP, bool isOutputP);

  executor::Vector<char> vector;
  bool isOutput;
};

}

#endif // GLOBAL_ARG_H_