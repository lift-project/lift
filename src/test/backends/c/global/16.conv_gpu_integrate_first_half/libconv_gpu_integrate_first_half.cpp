
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

      ; 
std::string kernel_string_3290;
cl::Program::Sources kernel_source_3290;
cl::Program kernel_program_3290;
cl::Kernel kernel_3290;
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
void lift_init(){
    
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
    kernel_string_3290 = readFile("kernel_3290.cl"); 
    kernel_source_3290 = cl::Program::Sources(1, {kernel_string_3290.c_str(), kernel_string_3290.length()}); 
    kernel_program_3290 = cl::Program(context, kernel_source_3290); 
    if ((kernel_program_3290.build({ device }) != CL_SUCCESS)){
        std::cerr << "kernel build error" << std::endl; 
        char* log; 
        size_t logsize; 
        assert((clGetProgramBuildInfo(kernel_program_3290.get(), device.get(), CL_PROGRAM_BUILD_LOG, 0, NULL, &logsize) == CL_SUCCESS)); 
        log = (char*)malloc(sizeof(char) * logsize); 
        assert((clGetProgramBuildInfo(kernel_program_3290.get(), device.get(), CL_PROGRAM_BUILD_LOG, logsize, log, NULL) == CL_SUCCESS)); 
        std::cout << log << std::endl; 
        exit(1); 
    }
    kernel_3290 = cl::Kernel(kernel_program_3290, "KERNEL"); 
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
    std::cout<<"func_name, cpu_time_ms, gpu_time_ms, diff_percentage"<<std::endl; 
}
void post_execute(){
    print_clock(); 
}

namespace lift {; 

void execute(float * v_initial_param_3281_2188, float * v_initial_param_3282_2189, float * & v_user_func_3291_2193, int v_inputChannels_1794, int v_kernelChannels_1796, int v_kernelWidthHeight_1795, int v_nInputs_1798, int v_inputWidthHeight_1797){
    // Allocate memory for output pointers
    cl::Buffer v_user_func_3287_2190(context, CL_MEM_READ_WRITE, ((v_inputChannels_1794 * v_kernelChannels_1796 * (int)pow((float)v_kernelWidthHeight_1795, 2)) * sizeof(float)));
    cl::Buffer v_user_func_3289_2191(context, CL_MEM_READ_WRITE, ((v_inputChannels_1794 * v_nInputs_1798 * (int)pow((float)v_inputWidthHeight_1797, 2)) * sizeof(float)));
    cl::Buffer v_user_func_3290_2192(context, CL_MEM_READ_WRITE, (((v_inputChannels_1794 * v_kernelChannels_1796 * v_nInputs_1798 * (int)pow((float)((v_inputWidthHeight_1797 + v_kernelStride_1800 + (-1 * v_kernelWidthHeight_1795)) / v_tileStride_1799), 2) * (int)pow((float)v_kernelWidthHeight_1795, 2) * (int)pow((float)(v_tileStride_1799 / v_kernelStride_1800), 2))/(v_seqOpsPerThread_1801)) * sizeof(float)));
    v_user_func_3291_2193 = reinterpret_cast<float *>(malloc((((v_inputChannels_1794 * v_kernelChannels_1796 * v_nInputs_1798 * (int)pow((float)((v_inputWidthHeight_1797 + v_kernelStride_1800 + (-1 * v_kernelWidthHeight_1795)) / v_tileStride_1799), 2) * (int)pow((float)v_kernelWidthHeight_1795, 2) * (int)pow((float)(v_tileStride_1799 / v_kernelStride_1800), 2))/(v_seqOpsPerThread_1801)) * sizeof(float)))); 
    ; 
    lift_queue.enqueueWriteBuffer(v_user_func_3287_2190, CL_TRUE, 0, ((v_inputChannels_1794 * v_kernelChannels_1796 * (int)pow((float)v_kernelWidthHeight_1795, 2)) * sizeof(float)), v_initial_param_3281_2188, NULL, NULL); 
    assert(lift_queue.finish() == CL_SUCCESS); 
    ; 
    ; 
    lift_queue.enqueueWriteBuffer(v_user_func_3289_2191, CL_TRUE, 0, ((v_inputChannels_1794 * v_nInputs_1798 * (int)pow((float)v_inputWidthHeight_1797, 2)) * sizeof(float)), v_initial_param_3282_2189, NULL, NULL); 
    assert(lift_queue.finish() == CL_SUCCESS); 
    ; 
    kernel_3290.setArg(0, v_user_func_3287_2190); 
    kernel_3290.setArg(1, v_user_func_3289_2191); 
    kernel_3290.setArg(2, v_user_func_3290_2192); 
    kernel_3290.setArg(3, v_inputChannels_1794); 
    kernel_3290.setArg(4, v_kernelChannels_1796); 
    kernel_3290.setArg(5, v_kernelWidthHeight_1795); 
    kernel_3290.setArg(6, v_nInputs_1798); 
    kernel_3290.setArg(7, v_inputWidthHeight_1797); 
    
    lift_queue.enqueueNDRangeKernel(kernel_3290, cl::NullRange, cl::NDRange(1,1,1), cl::NDRange(1,1,1), NULL, NULL); 
    ; 
    
    ; 
    lift_queue.enqueueReadBuffer(v_user_func_3290_2192, CL_TRUE, 0, (((v_inputChannels_1794 * v_kernelChannels_1796 * v_nInputs_1798 * (int)pow((float)((v_inputWidthHeight_1797 + v_kernelStride_1800 + (-1 * v_kernelWidthHeight_1795)) / v_tileStride_1799), 2) * (int)pow((float)v_kernelWidthHeight_1795, 2) * (int)pow((float)(v_tileStride_1799 / v_kernelStride_1800), 2))/(v_seqOpsPerThread_1801)) * sizeof(float)), v_user_func_3291_2193, NULL, NULL); 
    assert(lift_queue.finish() == CL_SUCCESS); 
    ; 
    post_execute(); 
}
}; 