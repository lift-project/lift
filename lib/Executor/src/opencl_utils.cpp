#include "opencl_utils.h"

cl::Context OpenCL::context;
cl::CommandQueue OpenCL::queue;
std::vector<cl::Device> OpenCL::devices;

cl_ulong OpenCL::device_local_mem_size = 0;
std::size_t OpenCL::device_max_work_group_size = 0;