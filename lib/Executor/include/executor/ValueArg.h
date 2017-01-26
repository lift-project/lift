///
/// \file ValueArg.h
///
/// \author Michel Steuwer <michel.steuwer@ed.ac.uk>
///


#ifndef VALUE_ARG_H_
#define VALUE_ARG_H_

#include <vector>

#include "Core.h"
#include "KernelArg.h"

namespace executor {

class ValueArg : public KernelArg {
public:
  static KernelArg* create(void* data, size_t sizeInBytes);

  void setAsKernelArg(cl::Kernel kernel, int i);
  void upload();
  void download();

private:
  ValueArg(std::vector<char>&& valueP);

  std::vector<char> value;
};

}

#endif // VALUE_ARG_H_