///
/// \file Executor.h
///
/// \author Michel Steuwer <michel.steuwer@ed.ac.uk>
///


#ifndef EXECUTOR_H_
#define EXECUTOR_H_

#include <string>
#include <vector>

#include "Core.h"
#include "Vector.h"
#include "DeviceList.h"

class KernelArg {
public:
  virtual ~KernelArg();

  virtual void setAsKernelArg(cl::Kernel kernel, int i) = 0;
  virtual void upload() = 0;
  virtual void download() = 0;
  virtual void clear();
};

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

std::istream& operator>>(std::istream& stream, KernelArg& arg);

std::ostream& operator<<(std::ostream& stream, const KernelArg& arg);

void initExecutor(int platformId, int deviceId);

void initExecutor(std::string deviceType = std::string("ANY"));

void shutdownExecutor();

std::string getPlatformName();

unsigned long getDeviceLocalMemSize();

unsigned long getDeviceGlobalMemSize();

unsigned long getDeviceMaxMemAllocSize();

unsigned long getDeviceMaxWorkGroupSize();

std::string getDeviceName();

std::string getDeviceType();

bool supportsDouble();

cl::Kernel buildKernel(const std::string& kernelCode,
                       const std::string& kernelName,
                       const std::string& buildOptions);

double executeKernel(cl::Kernel kernel,
               int localSize1, int localSize2, int localSize3,
               int globalSize1, int globalSize2, int globalSize3,
               const std::vector<KernelArg*>& args);

double execute(const std::string& kernelCode, const std::string& kernelName,
               const std::string& buildOptions,
               int localSize1, int localSize2, int localSize3,
               int globalSize1, int globalSize2, int globalSize3,
               const std::vector<KernelArg*>& args);

double benchmark(const std::string& kernelCode, const std::string& kernelName,
               const std::string& buildOptions,
               int localSize1, int localSize2, int localSize3,
               int globalSize1, int globalSize2, int globalSize3,
               const std::vector<KernelArg*>& args,
               int iterations, double timeout);

double evaluate(const std::string& kernelCode, const std::string& kernelName,
                const std::string& buildOptions,
                int localSize1, int localSize2, int localSize3,
                int globalSize1, int globalSize2, int globalSize3,
                const std::vector<KernelArg*>& args,
                int iterations, double timeout);

#endif // EXECUTOR_H_
