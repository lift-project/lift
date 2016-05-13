#pragma once

#include <algorithm>
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
  static std::vector<size_t> device_max_work_item_sizes;

public:
  static float timeout;
  static bool local_combinations;
  static size_t min_local_size;

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
    device_max_work_item_sizes = device.getInfo<CL_DEVICE_MAX_WORK_ITEM_SIZES>();

    std::cout << "Executing on " << device.getInfo<CL_DEVICE_NAME>() << std::endl;
  }

  static cl::Buffer alloc(cl_mem_flags flags, std::size_t size, void* data = nullptr)
  {
    return  cl::Buffer(context, flags, size, data);
  }

  template<typename T>
  static void executeRun(Run& run,
                         cl::NDRange local_size,
                         cl::Buffer output,
                         std::size_t output_size,
                         std::function<bool(const std::vector<T>&)> validation)
  {
    using namespace std;
    static int counter = 0; counter++;
    static double best_time = timeout;

    static const int iterations = 10;

    auto locals = (const ::size_t*) local_size;
    auto total_local_size = locals[0] * locals[1] * locals[2];

    if (total_local_size != 0 && total_local_size < min_local_size)
      return;

    std::vector<double> times; times.reserve(iterations);
    try {
      // prepare the kernel for execution
      run.setup(context);

      // executing
      cl::Event evt;
      for(int i = 0; i < iterations; ++i) {
        queue.enqueueNDRangeKernel(run.kernel, cl::NullRange,
                                   {run.glob1, run.glob2, run.glob3},
                                   local_size, nullptr, &evt);
        evt.wait();
        auto time = evt.getProfilingInfo<CL_PROFILING_COMMAND_END>() - evt.getProfilingInfo<CL_PROFILING_COMMAND_START>();
        times.push_back( ((double)time)/1000.0/1000.0 );
        if(times.back() > 5*best_time) break;
      }
      // read back the result
      std::vector<T> result(output_size);
      queue.enqueueReadBuffer(output, CL_TRUE, 0, result.size()*sizeof(T), result.data());

      if(!validation(result)) {
        // Save result to file
        File::add_invalid(run.hash);
        std::cerr << "[" << counter << "] Cross validation failed for " << run.hash << endl;
      }
      else {
        // take median
        sort(times.begin(), times.end());
        auto median = times[times.size()/2];
        // Save result to file
        File::add_time(run.hash, median, local_size);
        best_time = min(best_time, median);
        std::cout << "[" << counter << "] best time: " << best_time << std::endl;
      }
    } catch (const cl::Error& err) {
      if (err.err() != CL_INVALID_WORK_GROUP_SIZE) {
        File::add_blacklist(run.hash);
        cerr << "execution failed: " << run.hash << endl;
        cerr << err.what() << " (" << err.err() << ")" << std::endl;
        exit(err.err());
      }
    }
  }

  template<typename T>
  static void executeRun(Run& run,
                         cl::Buffer output,
                         std::size_t output_size,
                         std::function<bool(const std::vector<T>&)> validation)
  {
    // if local size is set in the run use this
    if (run.loc1 != 0 && run.loc2 != 0 && run.loc3 != 0) {
      executeRun(run, {run.loc1, run.loc2, run.loc3}, output, output_size, validation);
    } else {

      if (!local_combinations) {
        // let the OpenCL runtime choose an appropriate local size
        executeRun(run, cl::NDRange(), output, output_size, validation);
      } else {

        // loop over valid combinations, while ignoring tiny work-groups
        // and small numbers of work-groups
        size_t min_workgroup_size = 4;

        size_t loc1_max = run.loc1 == 0 ? device_max_work_item_sizes[0] : run.loc1;
        size_t loc2_max = run.loc2 == 0 ? device_max_work_item_sizes[1] : run.loc2;
        size_t loc3_max = run.loc3 == 0 ? device_max_work_item_sizes[2] : run.loc3;

        for (size_t loc1 = 1; loc1 <= loc1_max; loc1 *= 2) {
          for (size_t loc2 = 1; loc2 <= loc2_max; loc2 *= 2) {
            for (size_t loc3 = 1; loc3 <= loc3_max; loc3 *= 2) {
              auto total_size =loc1*loc2*loc3;
              if (total_size >= min_workgroup_size &&
                  total_size <= device_max_work_group_size &&
                  loc1 <= run.glob1 && loc2 <= run.glob2 && loc3 <= run.glob3)
                executeRun(run, {loc1, loc2, loc3}, output, output_size, validation);
            }
          }
        }
      }
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
          File::add_compileerror(hash);
        std::cerr << "Compilation failed: " << what << std::endl;
      }
        // the getBuildInfo might also fail
      catch (const cl::Error& err) {
        File::add_compileerror(hash);
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
