#include "Executor.h"
#include <algorithm>
#include <cassert>

namespace {
 
double getRuntimeInMilliseconds(cl::Event event)
{
  cl_ulong start;
  cl_ulong end;
  cl_int err;

  event.wait();

  err = clGetEventProfilingInfo(event(), CL_PROFILING_COMMAND_START,
                                sizeof(start), &start, NULL);
  ASSERT(err == CL_SUCCESS);

  err = clGetEventProfilingInfo(event(), CL_PROFILING_COMMAND_END,
                                sizeof(end), &end, NULL);
  ASSERT(err == CL_SUCCESS);

  return (end - start) * 1.0e-06;
}

}

KernelArg::~KernelArg()
{
}

void KernelArg::clear()
{
}

GlobalArg::GlobalArg(executor::Vector<char>&& vectorP, bool isOutputP)
  : vector(std::move(vectorP)), isOutput(isOutputP)
{
}

KernelArg* GlobalArg::create(void* data, size_t size, bool isOutput)
{
  auto dataCharPtr = static_cast<char*>(data);
  return new GlobalArg{executor::Vector<char>{dataCharPtr, dataCharPtr+size},
                       isOutput};
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
  kernel.setArg(i, cl::__local(size)); 
}

void LocalArg::upload() {}
void LocalArg::download() {}

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

void initExecutor(int platformId, int deviceId)
{
  executor::init(executor::platform(platformId), executor::device(deviceId));
  executor::defaultLogger.setLoggingLevel(executor::Logger::Severity::Warning);
}

void initExecutor(std::string deviceTypeString)
{
  executor::device_type deviceType;
  std::istringstream(deviceTypeString) >> deviceType;
  executor::init(executor::nDevices(1).deviceType(deviceType));
  executor::defaultLogger.setLoggingLevel(executor::Logger::Severity::Warning);
}

void shutdownExecutor()
{
  executor::terminate();
}

std::string getPlatformName()
{
  auto& devicePtr = executor::globalDeviceList.front();
  return devicePtr->clPlatform().getInfo<CL_PLATFORM_NAME>();
}

unsigned long getDeviceLocalMemSize()
{
  auto& devicePtr = executor::globalDeviceList.front();
  return devicePtr->localMemSize();
}

unsigned long getDeviceGlobalMemSize()
{
  auto& devicePtr = executor::globalDeviceList.front();
  return devicePtr->globalMemSize();
}

unsigned long getDeviceMaxMemAllocSize()
{
  auto& devicePtr = executor::globalDeviceList.front();
  return devicePtr->maxMemAllocSize();
}

unsigned long getDeviceMaxWorkGroupSize()
{
  auto& devicePtr = executor::globalDeviceList.front();
  return devicePtr->maxWorkGroupSize();
}

std::string getDeviceName()
{
  auto& devicePtr = executor::globalDeviceList.front();
  return devicePtr->name();
}

std::string getDeviceType()
{
  auto& devicePtr = executor::globalDeviceList.front();
  return devicePtr->typeAsString();
}

bool supportsDouble()
{
  auto& devicePtr = executor::globalDeviceList.front();
  return devicePtr->supportsDouble();
}

cl::Kernel buildKernel(const std::string& kernelCode,
                       const std::string& kernelName,
                       const std::string& buildOptions)
{
  auto& devPtr = executor::globalDeviceList.front();

  auto p = cl::Program(devPtr->clContext(), cl::Program::Sources(1, std::make_pair(kernelCode.c_str(), kernelCode.length())));

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

double executeKernel(cl::Kernel kernel,
               int localSize1, int localSize2, int localSize3,
               int globalSize1, int globalSize2, int globalSize3,
               const std::vector<KernelArg*>& args)
{
  auto& devPtr = executor::globalDeviceList.front();

  cl_uint clLocalSize1 = localSize1;
  cl_uint clGlobalSize1 = globalSize1;
  cl_uint clLocalSize2 = localSize2;
  cl_uint clGlobalSize2 = globalSize2;
  cl_uint clLocalSize3 = localSize3;
  cl_uint clGlobalSize3 = globalSize3;

  int i = 0;
  for (auto& arg : args) {
    arg->upload();
    arg->setAsKernelArg(kernel, i);
    ++i;
  }

  auto event = devPtr->enqueue(kernel,
                               cl::NDRange(clGlobalSize1,
                                           clGlobalSize2, clGlobalSize3),
                               cl::NDRange(clLocalSize1,
                                           clLocalSize2, clLocalSize3));

  for (auto& arg : args) arg->download();

  return getRuntimeInMilliseconds(event);
}

double execute(const std::string& kernelCode, const std::string& kernelName,
               const std::string& buildOptions,
               int localSize1, int localSize2, int localSize3,
               int globalSize1, int globalSize2, int globalSize3,
               const std::vector<KernelArg*>& args)
{
  auto kernel = buildKernel(kernelCode, kernelName, buildOptions);
  return executeKernel(kernel, localSize1, localSize2, localSize3,
                       globalSize1, globalSize2, globalSize3, args);
}

double benchmark(const std::string& kernelCode, const std::string& kernelName,
               const std::string& buildOptions,
               int localSize1, int localSize2, int localSize3,
               int globalSize1, int globalSize2, int globalSize3,
               const std::vector<KernelArg*>& args,
               int iterations, double timeout)
{
  auto kernel = buildKernel(kernelCode, kernelName, buildOptions);

  double *allRuntimes = new double[iterations];

  for (int i = 0; i < iterations; i++) {
    //std::cout << "Iteration: " << i << '\n';

    for(auto& arg:args){
      arg->clear();
    }

    double runtime = executeKernel(kernel, localSize1, localSize2, localSize3,
                       globalSize1, globalSize2, globalSize3, args);
    

    allRuntimes[i] = runtime;

    //std::cout << "Runtime: " << runtime << " ms\n";

    if (runtime >= timeout) {
      delete[] allRuntimes;
      return runtime;
    }
  }

  std::sort(allRuntimes, allRuntimes + iterations);

  double median;

  if (iterations % 2 == 0)
    median = (allRuntimes[iterations/2] + allRuntimes[iterations/2 - 1]) / 2.0;
  else 
    median = allRuntimes[iterations/2];

  delete[] allRuntimes;

  return median;
}

double evaluate(const std::string& kernelCode, const std::string& kernelName,
                const std::string& buildOptions,
                int localSize1, int localSize2, int localSize3,
                int globalSize1, int globalSize2, int globalSize3,
                const std::vector<KernelArg*>& args,
                int iterations, double timeout)
{
  auto& devPtr = executor::globalDeviceList.front();
  cl_uint clLocalSize1 = localSize1;
  cl_uint clGlobalSize1 = globalSize1;
  cl_uint clLocalSize2 = localSize2;
  cl_uint clGlobalSize2 = globalSize2;
  cl_uint clLocalSize3 = localSize3;
  cl_uint clGlobalSize3 = globalSize3;

  cl_int err = CL_SUCCESS;

  // Copy the buffers only once
  for (auto& arg : args) {
    arg->upload();
  }

  { // run a single workgroup on dummy data
    auto kernel = buildKernel(
        std::string{"#define WORKGROUP_GUARD {for(int i = 0; i < get_work_dim(); ++i) if(get_group_id(i)!=0) return;}\n"} + kernelCode,
           kernelName, buildOptions
        );

    auto wg_size = -1;
    err = kernel.getWorkGroupInfo(devPtr->clDevice(), CL_KERNEL_WORK_GROUP_SIZE ,&wg_size);
    if(err != CL_SUCCESS) {
      std::cerr << "ERROR " << err << std::endl;
      return -1;
    }

    cl_ulong private_mem = -1;
    err = kernel.getWorkGroupInfo(devPtr->clDevice(), CL_KERNEL_PRIVATE_MEM_SIZE, &private_mem);
    if(err != CL_SUCCESS) {
      std::cerr << "ERROR " << err << std::endl;
      return -1;
    }

   std::cout << "Amount of private memory: " << private_mem << std::endl;

    

    if(wg_size < localSize1 * localSize2 * localSize3) {
      return -1;
    }
    
    int i = 0;
    for (auto& arg : args) {
      arg->setAsKernelArg(kernel, i);
      ++i;
    }

    auto event = devPtr->enqueue(kernel,
                                 cl::NDRange(clGlobalSize1,
                                             clGlobalSize2, clGlobalSize3),
                                 cl::NDRange(clLocalSize1,
                                             clLocalSize2, clLocalSize3));

    auto time = getRuntimeInMilliseconds(event);
    std::cout << "Single workgroup: " << time << std::endl;
    if (time > timeout) return -time;
  }


  { // actual run
    std::vector<double> allRuntimes;
    allRuntimes.resize(iterations);

    auto kernel = buildKernel(kernelCode, kernelName, buildOptions); 
    int i = 0;
    for (auto& arg : args) {
      arg->setAsKernelArg(kernel, i);
      ++i;
    }

    for (int i = 0; i < iterations; i++) {
      auto event = devPtr->enqueue(kernel,
                                   cl::NDRange(clGlobalSize1,
                                               clGlobalSize2, clGlobalSize3),
                                   cl::NDRange(clLocalSize1,
                                               clLocalSize2, clLocalSize3));
      auto runtime = getRuntimeInMilliseconds(event);
      if(runtime > timeout) {
        for (auto& arg : args) arg->download();
        return runtime;
      }
      allRuntimes[i] = runtime;
    }
  
    for (auto& arg : args) arg->download();

    std::sort(std::begin(allRuntimes), std::end(allRuntimes));

    double median;

    if (iterations % 2 == 0)
      median = (allRuntimes[iterations/2] + allRuntimes[iterations/2 - 1]) / 2.0;
    else 
      median = allRuntimes[iterations/2];

    return median;
  }
}
