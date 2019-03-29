
#include <bits/stdc++.h>

using namespace std;

    

#include <iostream>
#include <CL/cl.hpp>
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
std::string kernel_string_605844;
cl::Program::Sources kernel_source_605844;
cl::Program kernel_program_605844;
cl::Kernel kernel_605844;
std::string kernel_string_605845;
cl::Program::Sources kernel_source_605845;
cl::Program kernel_program_605845;
cl::Kernel kernel_605845;
std::chrono::milliseconds cpu_clock_start_605845;
std::chrono::milliseconds cpu_clock_end_605845;
cl::Event event_605845;
; 
; 
; 
std::chrono::milliseconds cpu_clock_start_605844;
std::chrono::milliseconds cpu_clock_end_605844;
cl::Event event_605844;
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
 std::cout << " No platforms found. Check OpenCL installation!" << std::endl;
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
    kernel_string_605844 = readFile("kernel_605844.cl"); 
    kernel_source_605844 = cl::Program::Sources(1, {kernel_string_605844.c_str(), kernel_string_605844.length()}); 
    kernel_program_605844 = cl::Program(context, kernel_source_605844); 
    if ((kernel_program_605844.build({ device }) != CL_SUCCESS)){
        std::cerr<<"kernel build error"<<std::endl; exit(1);; 
    }
    kernel_605844 = cl::Kernel(kernel_program_605844, "KERNEL"); 
    kernel_string_605845 = readFile("kernel_605845.cl"); 
    kernel_source_605845 = cl::Program::Sources(1, {kernel_string_605845.c_str(), kernel_string_605845.length()}); 
    kernel_program_605845 = cl::Program(context, kernel_source_605845); 
    if ((kernel_program_605845.build({ device }) != CL_SUCCESS)){
        std::cerr<<"kernel build error"<<std::endl; exit(1);; 
    }
    kernel_605845 = cl::Kernel(kernel_program_605845, "KERNEL"); 
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
        double cpu_time_ms = cpu_time_in_ms(cpu_clock_start_605845, cpu_clock_end_605845);
        double gpu_time_ms = gpu_time_in_ms(event_605845);
        double diff_pc = diff_percent(cpu_time_ms, gpu_time_ms);
        std::cerr<<"OclFunCall_605845"<<","<<cpu_time_ms<<","<<gpu_time_ms<<","<<diff_pc<<std::endl; 
    }
    {
        double cpu_time_ms = cpu_time_in_ms(cpu_clock_start_605844, cpu_clock_end_605844);
        double gpu_time_ms = gpu_time_in_ms(event_605844);
        double diff_pc = diff_percent(cpu_time_ms, gpu_time_ms);
        std::cerr<<"OclFunCall_605844"<<","<<cpu_time_ms<<","<<gpu_time_ms<<","<<diff_pc<<std::endl; 
    }
}
void post_execute(){
    print_clock(); 
}


void execute(float * v_initial_param_605830_204992, float * v_initial_param_605831_204993, float * v_initial_param_605832_204994, float * & v_user_func_605846_205000){
    // Allocate memory for output pointers
    cl::Buffer v_user_func_605845_204999(context, CL_MEM_READ_WRITE, (216 * sizeof(float)));
    cl::Buffer v_user_func_605843_204997(context, CL_MEM_READ_WRITE, (256 * sizeof(float)));
    cl::Buffer v_user_func_605841_204996(context, CL_MEM_READ_WRITE, (54 * sizeof(float)));
    cl::Buffer v_user_func_605837_204995(context, CL_MEM_READ_WRITE, (3 * sizeof(float)));
    v_user_func_605846_205000 = reinterpret_cast<float *>(malloc((216 * sizeof(float)))); 
    cl::Buffer v_user_func_605844_204998(context, CL_MEM_READ_WRITE, (3888 * sizeof(float)));
    ; 
    lift_queue.enqueueWriteBuffer(v_user_func_605837_204995, CL_TRUE, 0, (3 * sizeof(float)), v_initial_param_605831_204993, NULL, NULL); 
    ; 
    ; 
    ; 
    lift_queue.enqueueWriteBuffer(v_user_func_605841_204996, CL_TRUE, 0, (54 * sizeof(float)), v_initial_param_605830_204992, NULL, NULL); 
    ; 
    ; 
    ; 
    lift_queue.enqueueWriteBuffer(v_user_func_605843_204997, CL_TRUE, 0, (256 * sizeof(float)), v_initial_param_605832_204994, NULL, NULL); 
    ; 
    ; 
    kernel_605844.setArg(0, v_user_func_605841_204996); 
    kernel_605844.setArg(1, v_user_func_605843_204997); 
    kernel_605844.setArg(2, v_user_func_605844_204998); 
    cpu_clock_start_605844 = std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::system_clock::now().time_since_epoch()); 
    lift_queue.enqueueNDRangeKernel(kernel_605844, cl::NullRange, cl::NDRange(1,1,1), cl::NDRange(1,1,1), NULL, (&event_605844)); 
    assert(lift_queue.finish() == CL_SUCCESS); 
    cpu_clock_end_605844 = std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::system_clock::now().time_since_epoch()); 
    kernel_605845.setArg(0, v_user_func_605837_204995); 
    kernel_605845.setArg(1, v_user_func_605844_204998); 
    kernel_605845.setArg(2, v_user_func_605845_204999); 
    cpu_clock_start_605845 = std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::system_clock::now().time_since_epoch()); 
    lift_queue.enqueueNDRangeKernel(kernel_605845, cl::NullRange, cl::NDRange(1,1,1), cl::NDRange(1,1,1), NULL, (&event_605845)); 
    assert(lift_queue.finish() == CL_SUCCESS); 
    cpu_clock_end_605845 = std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::system_clock::now().time_since_epoch()); 
    ; 
    lift_queue.enqueueReadBuffer(v_user_func_605845_204999, CL_TRUE, 0, (216 * sizeof(float)), v_user_func_605846_205000, NULL, NULL); 
    ; 
    ; 
    post_execute(); 
}