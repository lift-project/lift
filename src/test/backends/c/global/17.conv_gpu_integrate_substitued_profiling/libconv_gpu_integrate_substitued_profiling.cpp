
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
std::string kernel_string_3946;
cl::Program::Sources kernel_source_3946;
cl::Program kernel_program_3946;
cl::Kernel kernel_3946;
std::string kernel_string_3947;
cl::Program::Sources kernel_source_3947;
cl::Program kernel_program_3947;
cl::Kernel kernel_3947;
std::chrono::milliseconds cpu_clock_start_3947;
std::chrono::milliseconds cpu_clock_end_3947;
cl::Event event_3947;
; 
; 
; 
std::chrono::milliseconds cpu_clock_start_3946;
std::chrono::milliseconds cpu_clock_end_3946;
cl::Event event_3946;
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
    kernel_string_3946 = readFile("kernel_3946.cl"); 
    kernel_source_3946 = cl::Program::Sources(1, {kernel_string_3946.c_str(), kernel_string_3946.length()}); 
    kernel_program_3946 = cl::Program(context, kernel_source_3946); 
    if ((kernel_program_3946.build({ device }) != CL_SUCCESS)){
        std::cerr << "kernel build error" << std::endl; 
        char* log; 
        size_t logsize; 
        assert((clGetProgramBuildInfo(kernel_program_3946.get(), device.get(), CL_PROGRAM_BUILD_LOG, 0, NULL, &logsize) == CL_SUCCESS)); 
        log = (char*)malloc(sizeof(char) * logsize); 
        assert((clGetProgramBuildInfo(kernel_program_3946.get(), device.get(), CL_PROGRAM_BUILD_LOG, logsize, log, NULL) == CL_SUCCESS)); 
        std::cout << log << std::endl; 
        exit(1); 
    }
    kernel_3946 = cl::Kernel(kernel_program_3946, "KERNEL"); 
    kernel_string_3947 = readFile("kernel_3947.cl"); 
    kernel_source_3947 = cl::Program::Sources(1, {kernel_string_3947.c_str(), kernel_string_3947.length()}); 
    kernel_program_3947 = cl::Program(context, kernel_source_3947); 
    if ((kernel_program_3947.build({ device }) != CL_SUCCESS)){
        std::cerr << "kernel build error" << std::endl; 
        char* log; 
        size_t logsize; 
        assert((clGetProgramBuildInfo(kernel_program_3947.get(), device.get(), CL_PROGRAM_BUILD_LOG, 0, NULL, &logsize) == CL_SUCCESS)); 
        log = (char*)malloc(sizeof(char) * logsize); 
        assert((clGetProgramBuildInfo(kernel_program_3947.get(), device.get(), CL_PROGRAM_BUILD_LOG, logsize, log, NULL) == CL_SUCCESS)); 
        std::cout << log << std::endl; 
        exit(1); 
    }
    kernel_3947 = cl::Kernel(kernel_program_3947, "KERNEL"); 
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
    {
        double cpu_time_ms = cpu_time_in_ms(cpu_clock_start_3947, cpu_clock_end_3947);
        double gpu_time_ms = gpu_time_in_ms(event_3947);
        double diff_pc = diff_percent(cpu_time_ms, gpu_time_ms);
        std::cout<<"OclFunCall_3947"<<", "<<cpu_time_ms<<", "<<gpu_time_ms<<", "<<diff_pc<<std::endl; 
    }
    {
        double cpu_time_ms = cpu_time_in_ms(cpu_clock_start_3946, cpu_clock_end_3946);
        double gpu_time_ms = gpu_time_in_ms(event_3946);
        double diff_pc = diff_percent(cpu_time_ms, gpu_time_ms);
        std::cout<<"OclFunCall_3946"<<", "<<cpu_time_ms<<", "<<gpu_time_ms<<", "<<diff_pc<<std::endl; 
    }
}
void post_execute(){
    print_clock(); 
}

namespace lift {; 

void execute(float * v_initial_param_3932_2404, float * v_initial_param_3933_2405, float * v_initial_param_3934_2406, float * & v_user_func_3948_2412){
    // Allocate memory for output pointers
    cl::Buffer v_user_func_3939_2407(context, CL_MEM_READ_WRITE, (3 * sizeof(float)));
    cl::Buffer v_user_func_3945_2409(context, CL_MEM_READ_WRITE, (256 * sizeof(float)));
    cl::Buffer v_user_func_3947_2411(context, CL_MEM_READ_WRITE, (216 * sizeof(float)));
    v_user_func_3948_2412 = reinterpret_cast<float *>(malloc((216 * sizeof(float)))); 
    cl::Buffer v_user_func_3943_2408(context, CL_MEM_READ_WRITE, (54 * sizeof(float)));
    cl::Buffer v_user_func_3946_2410(context, CL_MEM_READ_WRITE, (3888 * sizeof(float)));
    ; 
    lift_queue.enqueueWriteBuffer(v_user_func_3939_2407, CL_TRUE, 0, (3 * sizeof(float)), v_initial_param_3933_2405, NULL, NULL); 
    assert(lift_queue.finish() == CL_SUCCESS); 
    ; 
    ; 
    lift_queue.enqueueWriteBuffer(v_user_func_3943_2408, CL_TRUE, 0, (54 * sizeof(float)), v_initial_param_3932_2404, NULL, NULL); 
    assert(lift_queue.finish() == CL_SUCCESS); 
    ; 
    ; 
    lift_queue.enqueueWriteBuffer(v_user_func_3945_2409, CL_TRUE, 0, (256 * sizeof(float)), v_initial_param_3934_2406, NULL, NULL); 
    assert(lift_queue.finish() == CL_SUCCESS); 
    ; 
    kernel_3946.setArg(0, v_user_func_3943_2408); 
    kernel_3946.setArg(1, v_user_func_3945_2409); 
    kernel_3946.setArg(2, v_user_func_3946_2410); 
    cpu_clock_start_3946 = std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::system_clock::now().time_since_epoch()); 
    lift_queue.enqueueNDRangeKernel(kernel_3946, cl::NullRange, cl::NDRange(1,1,1), cl::NDRange(1,1,1), NULL, (&event_3946)); 
    assert(lift_queue.finish() == CL_SUCCESS); 
    cpu_clock_end_3946 = std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::system_clock::now().time_since_epoch()); 
    kernel_3947.setArg(0, v_user_func_3939_2407); 
    kernel_3947.setArg(1, v_user_func_3946_2410); 
    kernel_3947.setArg(2, v_user_func_3947_2411); 
    cpu_clock_start_3947 = std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::system_clock::now().time_since_epoch()); 
    lift_queue.enqueueNDRangeKernel(kernel_3947, cl::NullRange, cl::NDRange(1,1,1), cl::NDRange(1,1,1), NULL, (&event_3947)); 
    assert(lift_queue.finish() == CL_SUCCESS); 
    cpu_clock_end_3947 = std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::system_clock::now().time_since_epoch()); 
    ; 
    lift_queue.enqueueReadBuffer(v_user_func_3947_2411, CL_TRUE, 0, (216 * sizeof(float)), v_user_func_3948_2412, NULL, NULL); 
    assert(lift_queue.finish() == CL_SUCCESS); 
    ; 
    post_execute(); 
}
}; 