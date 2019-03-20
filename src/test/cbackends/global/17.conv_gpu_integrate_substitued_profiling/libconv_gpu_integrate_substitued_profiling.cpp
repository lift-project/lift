
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
std::string kernel_string_363;
cl::Program::Sources kernel_source_363;
cl::Program kernel_program_363;
cl::Kernel kernel_363;
std::string kernel_string_364;
cl::Program::Sources kernel_source_364;
cl::Program kernel_program_364;
cl::Kernel kernel_364;
std::chrono::milliseconds cpu_clock_start_364;
std::chrono::milliseconds cpu_clock_end_364;
cl::Event event_364;
; 
; 
; 
std::chrono::milliseconds cpu_clock_start_363;
std::chrono::milliseconds cpu_clock_end_363;
cl::Event event_363;
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
    kernel_string_363 = readFile("kernel_363.cl"); 
    kernel_source_363 = cl::Program::Sources(1, {kernel_string_363.c_str(), kernel_string_363.length()}); 
    kernel_program_363 = cl::Program(context, kernel_source_363); 
    if ((kernel_program_363.build({ device }) != CL_SUCCESS)){
        std::cerr<<"kernel build error"<<std::endl; exit(1);; 
    }
    kernel_363 = cl::Kernel(kernel_program_363, "KERNEL"); 
    kernel_string_364 = readFile("kernel_364.cl"); 
    kernel_source_364 = cl::Program::Sources(1, {kernel_string_364.c_str(), kernel_string_364.length()}); 
    kernel_program_364 = cl::Program(context, kernel_source_364); 
    if ((kernel_program_364.build({ device }) != CL_SUCCESS)){
        std::cerr<<"kernel build error"<<std::endl; exit(1);; 
    }
    kernel_364 = cl::Kernel(kernel_program_364, "KERNEL"); 
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
        double cpu_time_ms = cpu_time_in_ms(cpu_clock_start_364, cpu_clock_end_364);
        double gpu_time_ms = gpu_time_in_ms(event_364);
        double diff_pc = diff_percent(cpu_time_ms, gpu_time_ms);
        std::cerr<<"OclFunCall_364"<<","<<cpu_time_ms<<","<<gpu_time_ms<<","<<diff_pc<<std::endl; 
    }
    {
        double cpu_time_ms = cpu_time_in_ms(cpu_clock_start_363, cpu_clock_end_363);
        double gpu_time_ms = gpu_time_in_ms(event_363);
        double diff_pc = diff_percent(cpu_time_ms, gpu_time_ms);
        std::cerr<<"OclFunCall_363"<<","<<cpu_time_ms<<","<<gpu_time_ms<<","<<diff_pc<<std::endl; 
    }
}
void post_execute(){
    print_clock(); 
}


void execute(float * v_initial_param_349_150, float * v_initial_param_350_151, float * v_initial_param_351_152, float * & v_user_func_365_158){
    // Allocate memory for output pointers
    cl::Buffer v_user_func_356_153(context, CL_MEM_READ_WRITE, (3 * sizeof(float)));
    cl::Buffer v_user_func_363_156(context, CL_MEM_READ_WRITE, (3888 * sizeof(float)));
    cl::Buffer v_user_func_364_157(context, CL_MEM_READ_WRITE, (216 * sizeof(float)));
    v_user_func_365_158 = reinterpret_cast<float *>(malloc((216 * sizeof(float)))); 
    cl::Buffer v_user_func_362_155(context, CL_MEM_READ_WRITE, (256 * sizeof(float)));
    cl::Buffer v_user_func_360_154(context, CL_MEM_READ_WRITE, (54 * sizeof(float)));
    ; 
    lift_queue.enqueueWriteBuffer(v_user_func_356_153, CL_TRUE, 0, (3 * sizeof(float)), v_initial_param_350_151, NULL, NULL); 
    ; 
    ; 
    ; 
    lift_queue.enqueueWriteBuffer(v_user_func_360_154, CL_TRUE, 0, (54 * sizeof(float)), v_initial_param_349_150, NULL, NULL); 
    ; 
    ; 
    ; 
    lift_queue.enqueueWriteBuffer(v_user_func_362_155, CL_TRUE, 0, (256 * sizeof(float)), v_initial_param_351_152, NULL, NULL); 
    ; 
    ; 
    kernel_363.setArg(0, v_user_func_360_154); 
    kernel_363.setArg(1, v_user_func_362_155); 
    kernel_363.setArg(2, v_user_func_363_156); 
    cpu_clock_start_363 = std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::system_clock::now().time_since_epoch()); 
    lift_queue.enqueueNDRangeKernel(kernel_363, cl::NullRange, cl::NDRange(1,1,1), cl::NDRange(1,1,1), NULL, (&event_363)); 
    assert(lift_queue.finish() == CL_SUCCESS); 
    cpu_clock_end_363 = std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::system_clock::now().time_since_epoch()); 
    kernel_364.setArg(0, v_user_func_356_153); 
    kernel_364.setArg(1, v_user_func_363_156); 
    kernel_364.setArg(2, v_user_func_364_157); 
    cpu_clock_start_364 = std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::system_clock::now().time_since_epoch()); 
    lift_queue.enqueueNDRangeKernel(kernel_364, cl::NullRange, cl::NDRange(1,1,1), cl::NDRange(1,1,1), NULL, (&event_364)); 
    assert(lift_queue.finish() == CL_SUCCESS); 
    cpu_clock_end_364 = std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::system_clock::now().time_since_epoch()); 
    ; 
    lift_queue.enqueueReadBuffer(v_user_func_364_157, CL_TRUE, 0, (216 * sizeof(float)), v_user_func_365_158, NULL, NULL); 
    ; 
    ; 
    post_execute(); 
}