#include "Kernel.h"

#include "DeviceList.h"
#include "util/Logger.h"

namespace executor {

Kernel::Kernel(std::string kernelSourceP, std::string kernelNameP, std::string buildOptionsP)
  : kernelSource(kernelSourceP), kernelName(kernelNameP), buildOptions(buildOptionsP), kernel()
{
}

Kernel* Kernel::create(std::string kernelSource, std::string kernelName, std::string buildOptions)
{
  return new Kernel{kernelSource, kernelName, buildOptions};
}

cl::Kernel Kernel::build() const
{
  if (kernel() != nullptr) return kernel;

  auto& devPtr = executor::globalDeviceList.front();

  auto p = cl::Program(devPtr->clContext(), cl::Program::Sources(1, std::make_pair(kernelSource.c_str(), kernelSource.length())));

  try {
    // build program for given device
    p.build(std::vector<cl::Device>(1, devPtr->clDevice()), buildOptions.c_str());

  } catch (cl::Error& err) {
    if (err.err() == CL_BUILD_PROGRAM_FAILURE) {
      LOG_ERROR(err);

      auto  buildLog = p.getBuildInfo<CL_PROGRAM_BUILD_LOG>(devPtr->clDevice() );
      LOG(executor::Logger::Severity::LogAlways, "Build log:\n", buildLog);
      
      ABORT_WITH_ERROR(err);
    } else {
      ABORT_WITH_ERROR(err);
    }
  }

  return cl::Kernel(p, kernelName.c_str());
}

std::string Kernel::getSource() const
{
  return kernelSource;
}

std::string Kernel::getName() const
{
  return kernelName;
}

std::string Kernel::getBuildOptions() const
{
  return buildOptions;
}

}