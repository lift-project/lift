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
#include "KernelArg.h"
#include "Kernel.h"

std::istream& operator>>(std::istream& stream, executor::KernelArg& arg);

std::ostream& operator<<(std::ostream& stream, const executor::KernelArg& arg);

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

double executeKernel(cl::Kernel kernel,
                     int localSize1, int localSize2, int localSize3,
                     int globalSize1, int globalSize2, int globalSize3,
                     const std::vector<executor::KernelArg*>& args);

double execute(const executor::Kernel& kernel,
               int localSize1, int localSize2, int localSize3,
               int globalSize1, int globalSize2, int globalSize3,
               const std::vector<executor::KernelArg*>& args);

void benchmark(const executor::Kernel& kernel,
               int localSize1, int localSize2, int localSize3,
               int globalSize1, int globalSize2, int globalSize3,
               const std::vector<executor::KernelArg*>& args,
               int iterations, double timeout,
               std::vector<double>& runtimes);

double evaluate(const executor::Kernel& kernel,
                int localSize1, int localSize2, int localSize3,
                int globalSize1, int globalSize2, int globalSize3,
                const std::vector<executor::KernelArg*>& args,
                int iterations, double timeout);

#endif // EXECUTOR_H_
