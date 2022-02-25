
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
std::string kernel_string_1639053527;
cl::Program::Sources kernel_source_1639053527;
cl::Program kernel_program_1639053527;
cl::Kernel kernel_1639053527;
std::chrono::milliseconds cpu_clock_start_1639053527;
std::chrono::milliseconds cpu_clock_end_1639053527;
cl::Event event_1639053527;
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
    kernel_string_1639053527 = readFile((pwd + "/kernel_1639053527.cl").c_str()); 
    kernel_source_1639053527 = cl::Program::Sources(1, {kernel_string_1639053527.c_str(), kernel_string_1639053527.length()}); 
    kernel_program_1639053527 = cl::Program(context, kernel_source_1639053527); 
    if ((kernel_program_1639053527.build({ device }) != CL_SUCCESS)){
        std::cerr << "kernel build error" << std::endl; 
        char* log; 
        size_t logsize; 
        assert((clGetProgramBuildInfo(kernel_program_1639053527.get(), device.get(), CL_PROGRAM_BUILD_LOG, 0, NULL, &logsize) == CL_SUCCESS)); 
        log = (char*)malloc(sizeof(char) * logsize); 
        assert((clGetProgramBuildInfo(kernel_program_1639053527.get(), device.get(), CL_PROGRAM_BUILD_LOG, logsize, log, NULL) == CL_SUCCESS)); 
        std::cerr << log << std::endl; 
        exit(1); 
    }
    kernel_1639053527 = cl::Kernel(kernel_program_1639053527, "KERNEL"); 
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
    {
        double cpu_time_ms = cpu_time_in_ms(cpu_clock_start_1639053527, cpu_clock_end_1639053527);
        double gpu_time_ms = gpu_time_in_ms(event_1639053527);
        double diff_pc = diff_percent(cpu_time_ms, gpu_time_ms);
        std::cerr<<"OclFunCall_1639053527"<<", "<<cpu_time_ms<<", "<<gpu_time_ms<<", "<<diff_pc<<std::endl; 
    }
}
void post_execute(){
    print_clock(); 
}

namespace lift {; 

void execute(float * v_initial_param_1639053536_622009642, float * v_initial_param_1639053535_622009643, float * & v_user_func_1639053526_622009647){
    // Allocate memory for output pointers
    cl::Buffer v__622009492(context, CL_MEM_READ_WRITE, (51380224 * sizeof(float)));
    cl::Buffer v_user_func_1639053530_622009644(context, CL_MEM_READ_WRITE, (131072 * sizeof(float)));
    cl::Buffer v_user_func_1639053527_622009646(context, CL_MEM_READ_WRITE, (100352 * sizeof(float)));
    cl::Buffer v_user_func_1639053528_622009645(context, CL_MEM_READ_WRITE, (2359296 * sizeof(float)));
    v_user_func_1639053526_622009647 = reinterpret_cast<float *>(malloc((100352 * sizeof(float)))); 
    ; 
    lift_queue.enqueueWriteBuffer(v_user_func_1639053530_622009644, CL_TRUE, 0, (131072 * sizeof(float)), v_initial_param_1639053536_622009642, NULL, NULL); 
    assert(lift_queue.finish() == CL_SUCCESS); 
    ; 
    ; 
    lift_queue.enqueueWriteBuffer(v_user_func_1639053528_622009645, CL_TRUE, 0, (2359296 * sizeof(float)), v_initial_param_1639053535_622009643, NULL, NULL); 
    assert(lift_queue.finish() == CL_SUCCESS); 
    ; 
    kernel_1639053527.setArg(0, v_user_func_1639053530_622009644); 
    kernel_1639053527.setArg(1, v_user_func_1639053528_622009645); 
    kernel_1639053527.setArg(2, v_user_func_1639053527_622009646); 
    kernel_1639053527.setArg(3, v__622009492); 
    cpu_clock_start_1639053527 = std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::system_clock::now().time_since_epoch()); 
    cerr << "thread block config(local): cl::NDRange(1,12,1)" << endl << "thread block config(global): cl::NDRange(512,12,1)" << endl;
    lift_queue.enqueueNDRangeKernel(kernel_1639053527, cl::NullRange, cl::NDRange(512,12,1), cl::NDRange(1,12,1), NULL, (&event_1639053527)); 
    assert(lift_queue.finish() == CL_SUCCESS); 
    cpu_clock_end_1639053527 = std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::system_clock::now().time_since_epoch()); 
    ; 
    lift_queue.enqueueReadBuffer(v_user_func_1639053527_622009646, CL_TRUE, 0, (100352 * sizeof(float)), v_user_func_1639053526_622009647, NULL, NULL); 
    assert(lift_queue.finish() == CL_SUCCESS); 
    ; 
    post_execute(); 
}
}; 