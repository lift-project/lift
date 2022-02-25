
#include <bits/stdc++.h>

using namespace std;

    

#include <iostream>
#include <CL/cl2.hpp>
#include <fstream>

std::string readFile(const char *filename){

  std::ifstream in(filename, std::ios::in);

  if (in.fail())
  {
  std::cerr << "Error reading file " << filename << std::endl;
  exit(1); }

  std::string contents;
  in.seekg(0, std::ios::end);
  contents.resize(in.tellg());
  in.seekg(0, std::ios::beg);
  in.read(&contents[0], contents.size());
  in.close();
  return contents;
  }

    

int platformId = 0;
int deviceId = 0;

 std::vector<cl::Platform> allPlatforms;
 cl::Platform platform;
 std::vector<cl::Device> allDevices;
 cl::Device device;
 cl::Context context;
 cl::CommandQueue lift_queue;

 size_t lift_global_0 = 1, lift_global_1 = 1, lift_global_2 =1;

      ; 
std::string kernel_string_431;
cl::Program::Sources kernel_source_431;
cl::Program kernel_program_431;
cl::Kernel kernel_431;
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
; 
void lift_init(const std::string & pwd){
    
	cl::Platform::get(&allPlatforms);
 if (allPlatforms.size() == 0) {
 std::cerr << " No platforms found. Check OpenCL installation!" << std::endl;
 exit(1);
 }

 platform = allPlatforms[platformId];

 platform.getDevices(CL_DEVICE_TYPE_ALL, &allDevices);
 if (allDevices.size() == 0) {
 std::cerr << " No devices found. Check OpenCL installation!" << std::endl;
 exit(1);
 }

 device = allDevices[deviceId];

 std::cerr << "Using platform: " << platform.getInfo<CL_PLATFORM_NAME>() << std::endl;
 std::cerr << "Using device: " << device.getInfo<CL_DEVICE_NAME>() << std::endl;

 // create context
 cl::Context tmp_context({ device });
 context = std::move(tmp_context);

 // create queue
 cl::CommandQueue tmp_queue(context, device, CL_QUEUE_PROFILING_ENABLE);
 lift_queue = std::move(tmp_queue);
      ; 
    kernel_string_431 = readFile((pwd + "/kernel_431.cl").c_str()); 
    kernel_source_431 = cl::Program::Sources(1, {kernel_string_431.c_str(), kernel_string_431.length()}); 
    kernel_program_431 = cl::Program(context, kernel_source_431); 
    if ((kernel_program_431.build({ device }) != CL_SUCCESS)){
        std::cerr << "kernel build error" << std::endl; 
        char* log; 
        size_t logsize; 
        assert((clGetProgramBuildInfo(kernel_program_431.get(), device.get(), CL_PROGRAM_BUILD_LOG, 0, NULL, &logsize) == CL_SUCCESS)); 
        log = (char*)malloc(sizeof(char) * logsize); 
        assert((clGetProgramBuildInfo(kernel_program_431.get(), device.get(), CL_PROGRAM_BUILD_LOG, logsize, log, NULL) == CL_SUCCESS)); 
        std::cout << log << std::endl; 
        exit(1); 
    }
    kernel_431 = cl::Kernel(kernel_program_431, "KERNEL"); 
}

double cpu_time_in_ms( std::chrono::milliseconds start, std::chrono::milliseconds finish ){
 return (finish - start).count();
}

double gpu_time_in_ms( cl::Event event ){

  cl_ulong start;
  cl_ulong end;
  cl_int err;

  event.wait();

  err = clGetEventProfilingInfo(event(), CL_PROFILING_COMMAND_START,
                                sizeof(start), &start, NULL);
  assert(err == CL_SUCCESS);

  err = clGetEventProfilingInfo(event(), CL_PROFILING_COMMAND_END,
                                sizeof(end), &end, NULL);
  assert(err == CL_SUCCESS);

  return ((double)(end - start)) * 1.0e-6;
}

double diff_percent(double lhs, double rhs) {
  if(std::min(lhs,rhs)==0)
    return -1;
  else
    return std::abs(lhs-rhs)/std::min(lhs,rhs);
}

          ; 
void print_clock(){
    std::cerr<<"func_name, cpu_time_ms, gpu_time_ms, diff_percentage"<<std::endl; 
}
void post_execute(){
    print_clock(); 
}

namespace lift {; 

void execute(float * v_initial_param_419_63, float * v_initial_param_420_64, float * v_initial_param_421_65, float * & v_user_func_432_70){
    // Allocate memory for output pointers
    cl::Buffer v_user_func_431_69(context, CL_MEM_READ_WRITE, (192 * sizeof(float)));
    cl::Buffer v_user_func_430_68(context, CL_MEM_READ_WRITE, (4 * sizeof(float)));
    cl::Buffer v_user_func_428_67(context, CL_MEM_READ_WRITE, (96 * sizeof(float)));
    v_user_func_432_70 = reinterpret_cast<float *>(malloc((192 * sizeof(float)))); 
    cl::Buffer v_user_func_426_66(context, CL_MEM_READ_WRITE, (243 * sizeof(float)));
    ; 
    lift_queue.enqueueWriteBuffer(v_user_func_426_66, CL_TRUE, 0, (243 * sizeof(float)), v_initial_param_419_63, NULL, NULL); 
    assert(lift_queue.finish() == CL_SUCCESS); 
    ; 
    ; 
    lift_queue.enqueueWriteBuffer(v_user_func_428_67, CL_TRUE, 0, (96 * sizeof(float)), v_initial_param_420_64, NULL, NULL); 
    assert(lift_queue.finish() == CL_SUCCESS); 
    ; 
    ; 
    lift_queue.enqueueWriteBuffer(v_user_func_430_68, CL_TRUE, 0, (4 * sizeof(float)), v_initial_param_421_65, NULL, NULL); 
    assert(lift_queue.finish() == CL_SUCCESS); 
    ; 
    kernel_431.setArg(0, v_user_func_426_66); 
    kernel_431.setArg(1, v_user_func_428_67); 
    kernel_431.setArg(2, v_user_func_430_68); 
    kernel_431.setArg(3, v_user_func_431_69); 
    
    cerr << "thread block config: "<< lift_global_0 << ',' << ' ' << lift_global_1 << ',' << ' ' << lift_global_2 << endl;
    lift_queue.enqueueNDRangeKernel(kernel_431, cl::NullRange, cl::NDRange(6,8,4), cl::NDRange(6,8,4), NULL, NULL); 
    ; 
    
    ; 
    lift_queue.enqueueReadBuffer(v_user_func_431_69, CL_TRUE, 0, (192 * sizeof(float)), v_user_func_432_70, NULL, NULL); 
    assert(lift_queue.finish() == CL_SUCCESS); 
    ; 
    post_execute(); 
}
}; 
