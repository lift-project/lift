
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
std::string kernel_string_1314;
cl::Program::Sources kernel_source_1314;
cl::Program kernel_program_1314;
cl::Kernel kernel_1314;
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
    kernel_string_1314 = readFile("kernel_1314.cl"); 
    kernel_source_1314 = cl::Program::Sources(1, {kernel_string_1314.c_str(), kernel_string_1314.length()}); 
    kernel_program_1314 = cl::Program(context, kernel_source_1314); 
    if ((kernel_program_1314.build({ device }) != CL_SUCCESS)){
        std::cerr<<"kernel build error"<<std::endl; exit(1);; 
    }
    kernel_1314 = cl::Kernel(kernel_program_1314, "KERNEL"); 
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


void execute(float * v_initial_param_1305_1579, float * v_initial_param_1306_1580, float * & v_user_func_1315_1584, int v_inputChannels_977, int v_kernelChannels_979, int v_kernelWidthHeight_978, int v_nInputs_981, int v_inputWidthHeight_980){
    // Allocate memory for output pointers
    cl::Buffer v_user_func_1311_1581(context, CL_MEM_READ_WRITE, ((v_inputChannels_977 * v_kernelChannels_979 * (int)pow((float)v_kernelWidthHeight_978, 2)) * sizeof(float)));
    cl::Buffer v_user_func_1313_1582(context, CL_MEM_READ_WRITE, ((v_inputChannels_977 * v_nInputs_981 * (int)pow((float)v_inputWidthHeight_980, 2)) * sizeof(float)));
    cl::Buffer v_user_func_1314_1583(context, CL_MEM_READ_WRITE, (((v_inputChannels_977 * v_kernelChannels_979 * v_nInputs_981 * (int)pow((float)((v_inputWidthHeight_980 + v_kernelStride_983 + (-1 * v_kernelWidthHeight_978)) / v_tileStride_982), 2) * (int)pow((float)v_kernelWidthHeight_978, 2) * (int)pow((float)(v_tileStride_982 / v_kernelStride_983), 2))/(v_seqOpsPerThread_984)) * sizeof(float)));
    v_user_func_1315_1584 = reinterpret_cast<float *>(malloc((((v_inputChannels_977 * v_kernelChannels_979 * v_nInputs_981 * (int)pow((float)((v_inputWidthHeight_980 + v_kernelStride_983 + (-1 * v_kernelWidthHeight_978)) / v_tileStride_982), 2) * (int)pow((float)v_kernelWidthHeight_978, 2) * (int)pow((float)(v_tileStride_982 / v_kernelStride_983), 2))/(v_seqOpsPerThread_984)) * sizeof(float)))); 
    ; 
    lift_queue.enqueueWriteBuffer(v_user_func_1311_1581, CL_TRUE, 0, ((v_inputChannels_977 * v_kernelChannels_979 * (int)pow((float)v_kernelWidthHeight_978, 2)) * sizeof(float)), v_initial_param_1305_1579, NULL, NULL); 
    ; 
    ; 
    ; 
    lift_queue.enqueueWriteBuffer(v_user_func_1313_1582, CL_TRUE, 0, ((v_inputChannels_977 * v_nInputs_981 * (int)pow((float)v_inputWidthHeight_980, 2)) * sizeof(float)), v_initial_param_1306_1580, NULL, NULL); 
    ; 
    ; 
    kernel_1314.setArg(0, v_user_func_1311_1581); 
    kernel_1314.setArg(1, v_user_func_1313_1582); 
    kernel_1314.setArg(2, v_user_func_1314_1583); 
    kernel_1314.setArg(3, v_inputChannels_977); 
    kernel_1314.setArg(4, v_kernelChannels_979); 
    kernel_1314.setArg(5, v_kernelWidthHeight_978); 
    kernel_1314.setArg(6, v_nInputs_981); 
    kernel_1314.setArg(7, v_inputWidthHeight_980); 
    
    lift_queue.enqueueNDRangeKernel(kernel_1314, cl::NullRange, cl::NDRange(1,1,1), cl::NDRange(1,1,1), NULL, NULL); 
    ; 
    
    ; 
    lift_queue.enqueueReadBuffer(v_user_func_1314_1583, CL_TRUE, 0, (((v_inputChannels_977 * v_kernelChannels_979 * v_nInputs_981 * (int)pow((float)((v_inputWidthHeight_980 + v_kernelStride_983 + (-1 * v_kernelWidthHeight_978)) / v_tileStride_982), 2) * (int)pow((float)v_kernelWidthHeight_978, 2) * (int)pow((float)(v_tileStride_982 / v_kernelStride_983), 2))/(v_seqOpsPerThread_984)) * sizeof(float)), v_user_func_1315_1584, NULL, NULL); 
    ; 
    ; 
    post_execute(); 
}