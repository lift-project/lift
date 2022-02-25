
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
std::string kernel_string_391113;
cl::Program::Sources kernel_source_391113;
cl::Program kernel_program_391113;
cl::Kernel kernel_391113;
std::chrono::milliseconds cpu_clock_start_391113;
std::chrono::milliseconds cpu_clock_end_391113;
cl::Event event_391113;
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
    kernel_string_391113 = readFile((pwd + "/kernel_391113.cl").c_str()); 
    kernel_source_391113 = cl::Program::Sources(1, {kernel_string_391113.c_str(), kernel_string_391113.length()}); 
    kernel_program_391113 = cl::Program(context, kernel_source_391113); 
    if ((kernel_program_391113.build({ device }) != CL_SUCCESS)){
        std::cerr << "kernel build error" << std::endl; 
        char* log; 
        size_t logsize; 
        assert((clGetProgramBuildInfo(kernel_program_391113.get(), device.get(), CL_PROGRAM_BUILD_LOG, 0, NULL, &logsize) == CL_SUCCESS)); 
        log = (char*)malloc(sizeof(char) * logsize); 
        assert((clGetProgramBuildInfo(kernel_program_391113.get(), device.get(), CL_PROGRAM_BUILD_LOG, logsize, log, NULL) == CL_SUCCESS)); 
        std::cerr << log << std::endl; 
        exit(1); 
    }
    kernel_391113 = cl::Kernel(kernel_program_391113, "KERNEL"); 
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
        double cpu_time_ms = cpu_time_in_ms(cpu_clock_start_391113, cpu_clock_end_391113);
        double gpu_time_ms = gpu_time_in_ms(event_391113);
        double diff_pc = diff_percent(cpu_time_ms, gpu_time_ms);
        std::cerr<<"OclFunCall_391113"<<", "<<cpu_time_ms<<", "<<gpu_time_ms<<", "<<diff_pc<<std::endl; 
    }
}
void post_execute(){
    print_clock(); 
}

namespace lift {; 

void execute(float * v_initial_param_391104_91999, float * v_initial_param_391105_92000, float * & v_user_func_391114_92004){
    // Allocate memory for output pointers
    cl::Buffer v_user_func_391110_92001(context, CL_MEM_READ_WRITE, (153228 * sizeof(float)));
    cl::Buffer v__91795(context, CL_MEM_READ_WRITE, (86704128 * sizeof(float)));
    cl::Buffer v__91791(context, CL_MEM_READ_WRITE, (86704128 * sizeof(float)));
    cl::Buffer v_user_func_391113_92003(context, CL_MEM_READ_WRITE, (3211264 * sizeof(float)));
    cl::Buffer v__91790(context, CL_MEM_READ_WRITE, (12096 * sizeof(float)));
    cl::Buffer v_user_func_391112_92002(context, CL_MEM_READ_WRITE, (1728 * sizeof(float)));
    v_user_func_391114_92004 = reinterpret_cast<float *>(malloc((3211264 * sizeof(float)))); 
    cl::Buffer v__91803(context, CL_MEM_READ_WRITE, (9633792 * sizeof(float)));
    ; 
    lift_queue.enqueueWriteBuffer(v_user_func_391110_92001, CL_TRUE, 0, (153228 * sizeof(float)), v_initial_param_391104_91999, NULL, NULL); 
    assert(lift_queue.finish() == CL_SUCCESS); 
    ; 
    ; 
    lift_queue.enqueueWriteBuffer(v_user_func_391112_92002, CL_TRUE, 0, (1728 * sizeof(float)), v_initial_param_391105_92000, NULL, NULL); 
    assert(lift_queue.finish() == CL_SUCCESS); 
    ; 
    kernel_391113.setArg(0, v_user_func_391110_92001); 
    kernel_391113.setArg(1, v_user_func_391112_92002); 
    kernel_391113.setArg(2, v_user_func_391113_92003); 
    kernel_391113.setArg(3, v__91790); 
    kernel_391113.setArg(4, v__91791); 
    kernel_391113.setArg(5, v__91795); 
    kernel_391113.setArg(6, v__91803); 
    cpu_clock_start_391113 = std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::system_clock::now().time_since_epoch()); 
    cerr << "thread block config(local): cl::NDRange(223,1,1)" << endl << "thread block config(global): cl::NDRange(14272,1,1)" << endl;
    lift_queue.enqueueNDRangeKernel(kernel_391113, cl::NullRange, cl::NDRange(14272,1,1), cl::NDRange(223,1,1), NULL, (&event_391113)); 
    assert(lift_queue.finish() == CL_SUCCESS); 
    cpu_clock_end_391113 = std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::system_clock::now().time_since_epoch()); 
    ; 
    lift_queue.enqueueReadBuffer(v_user_func_391113_92003, CL_TRUE, 0, (3211264 * sizeof(float)), v_user_func_391114_92004, NULL, NULL); 
    assert(lift_queue.finish() == CL_SUCCESS); 
    ; 
    post_execute(); 
}
}; 