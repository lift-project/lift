#include "LocalArg.h"

#include "util/Logger.h"

namespace executor {

LocalArg::LocalArg(size_t sizeP)
  : size(sizeP)
{
}

KernelArg* LocalArg::create(size_t size)
{
  return new LocalArg{size};
}

void LocalArg::setAsKernelArg(cl::Kernel kernel, int i)
{
  LOG_DEBUG_INFO("Setting LocalArg with size ", size, ", at position ", i);
  kernel.setArg(i, cl::__local(size)); 
}

void LocalArg::upload() {}
void LocalArg::download() {}

}
