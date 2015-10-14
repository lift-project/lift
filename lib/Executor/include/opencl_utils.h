#pragma once

#include <vector>
#include <functional>
#include <cassert>
#include <iostream>

#define __CL_ENABLE_EXCEPTIONS
#include <CL/cl.hpp>

#include "run.h"
#include "file_utils.h"

class OpenCL {
  static cl::Context context;
  static cl::CommandQueue queue;
  static std::vector<cl::Device> devices;

  static cl_ulong device_local_mem_size;
  static std::size_t device_max_work_group_size;

public:
  static float timeout;

  static void init(const unsigned platform_idx, const unsigned device_idx)
  {
    std::vector<cl::Platform> platform;
    cl::Platform::get(&platform);

    assert(platform.size() >= platform_idx+1 && "platform not found");

    platform[platform_idx].getDevices(CL_DEVICE_TYPE_ALL, &devices);
    assert(devices.size() >= device_idx+1 && "Device not found");

    devices = {devices[device_idx]};
    context = cl::Context(devices);
    auto &device = devices.front();
    queue = cl::CommandQueue(context, device, CL_QUEUE_PROFILING_ENABLE);

    device_local_mem_size = device.getInfo<CL_DEVICE_LOCAL_MEM_SIZE>();
    device_max_work_group_size = device.getInfo<CL_DEVICE_MAX_WORK_GROUP_SIZE>();

    std::cout << "Executing on " << device.getInfo<CL_DEVICE_NAME>() << std::endl;
  }

  static cl::Buffer alloc(cl_mem_flags flags, std::size_t size, void* data = nullptr)
  {
    return  cl::Buffer(context, flags, size, data);
  }

  template<typename T>
  static void executeRun(Run& run,
                         cl::Buffer output,
                         std::size_t output_size,
                         std::function<bool(const std::vector<T>&)> validation)
  {
    using namespace std;
    static int counter = 0; counter++;
    static double best_time = timeout;
    try {
      // prepare the kernel for execution
      run.setup(context);

      // executing
      cl::Event evt;
      for(int i = 0; i < 5; ++i) {
        queue.enqueueNDRangeKernel(run.kernel, cl::NullRange,
                                   {run.glob1, run.glob2, run.glob3},
                                   {run.loc1, run.loc2, run.loc3}, nullptr, &evt);
        evt.wait();
        auto time = evt.getProfilingInfo<CL_PROFILING_COMMAND_END>() - evt.getProfilingInfo<CL_PROFILING_COMMAND_START>();
        double ms = ((double)time)/1000.0/1000.0;
        if(ms > 1.2*best_time) break;
      }
      // read back the result
      std::vector<T> result(output_size);
      queue.enqueueReadBuffer(output, CL_TRUE, 0, result.size()*sizeof(float), result.data());
      auto time = evt.getProfilingInfo<CL_PROFILING_COMMAND_END>() - evt.getProfilingInfo<CL_PROFILING_COMMAND_START>();
      double ms = ((double)time)/1000.0/1000.0;

      if(!validation(result)) {
        // Save result to file
        File::add_invalid(run.hash);
        std::cerr << "[" << counter << "] Cross validation failed for " << run.hash << endl;
      }
      else {
        // Save result to file
        File::add_time(run.hash, ms);
        best_time = min(best_time, ms);
        std::cout << "[" << counter << "] best time: " << best_time << std::endl;
      }
    } catch (const cl::Error& err) {
      File::add_blacklist(run.hash);
      cerr << "execution failed: " << run.hash << endl;
      cerr << err.what() << " (" << err.err() << ")" << std::endl;
      exit(err.err());
    }
  }

  static bool compatibility_checks(cl::Kernel &kernel, size_t num_work_items) {
    size_t wg_size = 0;
    cl_ulong local_size = 0;

    kernel.getWorkGroupInfo(devices.front(), CL_KERNEL_WORK_GROUP_SIZE ,&wg_size);
    kernel.getWorkGroupInfo(devices.front(), CL_KERNEL_LOCAL_MEM_SIZE ,&local_size);

    if(wg_size == 0 || num_work_items > device_max_work_group_size) return false;
    if(local_size > device_local_mem_size) return false;

    return true;
  };

  static bool compile(const std::string& hash,
               cl::Kernel& kernel,
               const unsigned num_work_items,
               const bool binary_mode)
  {
    auto code = loadCode(hash, binary_mode);
    if(code.size() == 0) {
      File::add_blacklist(hash);
      return false;
    }
    cl::Program program;

    try {
      if(binary_mode)
        program = cl::Program(context, devices, cl::Program::Binaries(1, std::make_pair(code.data(), code.size())));
      else
        program = cl::Program(context, cl::Program::Sources(1, std::make_pair(code.data(), code.size())));
      program.build(devices);
      kernel = cl::Kernel(program, "KERNEL");
      if(!compatibility_checks(kernel, num_work_items)) {
        kernel = cl::Kernel();
        File::add_incompatible(hash);
        return false;
      }
    } catch (const cl::Error& err) {
      try {
        const std::string what = program.getBuildInfo<CL_PROGRAM_BUILD_LOG>(devices.front());
        // Nvidia doesn't compile the code if it uses too much memory (ptxas error)
        if (what.find("uses too much shared data") != std::string::npos)
          File::add_incompatible(hash);
        else
          File::add_blacklist(hash);
        std::cerr << "Compilation failed: " << what << std::endl;
      }
        // the getBuildInfo might also fail
      catch (const cl::Error& err) {
        File::add_blacklist(hash);
      }
      return false;
    }
    return true;
  }

protected:
  static std::string loadCode(const std::string& hash, bool binary)
  {
    std::ifstream t(hash + (binary ? ".bin" : ".cl"));
    std::string cl_code = std::string((std::istreambuf_iterator<char>(t)),
                                      std::istreambuf_iterator<char>());
    if(cl_code.size() == 0) {
      std::cerr << "\nNo source for " << hash << ".cl\n" << std::endl;
      File::add_blacklist(hash);
    }

    return cl_code;
  }


};