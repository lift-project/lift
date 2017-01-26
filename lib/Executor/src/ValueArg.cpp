#include "ValueArg.h"

namespace executor {

ValueArg::ValueArg(std::vector<char>&& valueP)
  : value(std::move(valueP))
{
}

KernelArg* ValueArg::create(void* data, size_t size)
{
  auto dataCharPtr = static_cast<char*>(data);
  std::vector<char> value(dataCharPtr, dataCharPtr+size);

  return new ValueArg{std::move(value)};
}

void ValueArg::setAsKernelArg(cl::Kernel kernel, int i)
{
  kernel.setArg(i, value.size(), value.data());
}

void ValueArg::upload() {}
void ValueArg::download() {}

}
