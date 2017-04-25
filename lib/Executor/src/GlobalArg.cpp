#include "GlobalArg.h"

#include "util/Logger.h"

namespace executor {

GlobalArg::GlobalArg(executor::Vector<char>&& vectorP, bool isOutputP)
  : vector(std::move(vectorP)), isOutput(isOutputP)
{
}

KernelArg* GlobalArg::create(void* data, size_t size, bool isOutput)
{
  auto dataCharPtr = static_cast<char*>(data);
  executor::Vector<char> vector(dataCharPtr, dataCharPtr+size);
  return new GlobalArg{std::move(vector), isOutput};
}

KernelArg* GlobalArg::create(size_t size, bool isOutput)
{
  executor::Vector<char> vector;
  vector.resize(size);
  return new GlobalArg{std::move(vector), isOutput};
}

const executor::Vector<char>& GlobalArg::data() const
{
  return vector;
}

void GlobalArg::clear()
{
  if(isOutput){
    vector.assign(vector.size());
    vector.dataOnHostModified();
  }
}

void GlobalArg::setAsKernelArg(cl::Kernel kernel, int i)
{
  LOG_DEBUG_INFO("Setting GlobalArg with size ", vector.size(), ", at position ", i);
  auto& devPtr = executor::globalDeviceList.front();
  kernel.setArg(i, vector.deviceBuffer(*devPtr).clBuffer());
}

void GlobalArg::upload()
{
  // create buffers on device
  vector.createDeviceBuffers();
  // start upload
  vector.startUpload();
}

void GlobalArg::download()
{
  if (isOutput) {
    vector.dataOnDeviceModified();
    vector.copyDataToHost();
  }
}

}
