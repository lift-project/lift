#include <vector>
#include <string>

#include "opencl_utils.h"
#include "run.h"

// Load the file and compile the program
bool Run::compile(bool binary_mode)
{
  return OpenCL::compile(hash, kernel, loc1*loc2*loc3, binary_mode);
}