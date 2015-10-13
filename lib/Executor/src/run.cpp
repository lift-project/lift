#include <vector>
#include <string>
#include <cassert>
#include <algorithm>
#include <fstream>

#include "file_utils.h"
#include "csv_utils.h"
#include "opencl_utils.h"
#include "run.h"

// Load the file and compile the program
bool Run::compile(cl::Context context,
                  std::vector<cl::Device> dev,
                  std::function<bool(cl::Kernel&,size_t,size_t,size_t)> compatibility,
                  bool binary_mode)
{
  auto code = loadCode(binary_mode);
  if(code.size() == 0) {
    File::add_blacklist(hash);
    return false;
  }
  cl::Program program;

  try {
    if(binary_mode)
      program = cl::Program(context, dev, cl::Program::Binaries(1, std::make_pair(code.data(), code.size())));
    else
      program = cl::Program(context, cl::Program::Sources(1, std::make_pair(code.data(), code.size())));
    program.build(dev);
    kernel = cl::Kernel(program, "KERNEL");
    if(!compatibility(kernel, loc1, loc2, loc3)) {
      kernel = cl::Kernel();
      File::add_incompatible(hash);
      return false;
    }
  } catch (const cl::Error& err) {
    try {
      const std::string what = program.getBuildInfo<CL_PROGRAM_BUILD_LOG>(dev[0]);
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

std::string Run::loadCode(bool binary)
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