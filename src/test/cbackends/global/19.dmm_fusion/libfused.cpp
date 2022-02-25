
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
std::string kernel_string_396;
cl::Program::Sources kernel_source_396;
cl::Program kernel_program_396;
cl::Kernel kernel_396;
std::chrono::milliseconds cpu_clock_start_396;
std::chrono::milliseconds cpu_clock_end_396;
cl::Event event_396;
std::chrono::milliseconds cpu_clock_start_391;
std::chrono::milliseconds cpu_clock_end_391;
cl::Event event_391;
std::chrono::milliseconds cpu_clock_start_393;
std::chrono::milliseconds cpu_clock_end_393;
cl::Event event_393;
std::chrono::milliseconds cpu_clock_start_395;
std::chrono::milliseconds cpu_clock_end_395;
cl::Event event_395;
std::chrono::milliseconds cpu_clock_start_397;
std::chrono::milliseconds cpu_clock_end_397;
cl::Event event_397;
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
	kernel_string_396 = readFile("kernel_396.cl"); 
	kernel_source_396 = cl::Program::Sources(1, {kernel_string_396.c_str(), kernel_string_396.length()}); 
	/* const char *option = "-cl-opt-disable"; */
	const char *option = NULL;
	kernel_program_396 = cl::Program(context, kernel_source_396); 
	if ((kernel_program_396.build({ device }, option) != CL_SUCCESS)){
		std::cerr << "kernel build error" << std::endl; 
		char* log; 
		size_t logsize; 
		assert((clGetProgramBuildInfo(kernel_program_396.get(), device.get(), CL_PROGRAM_BUILD_LOG, 0, NULL, &logsize) == CL_SUCCESS)); 
		log = (char*)malloc(sizeof(char) * logsize); 
		assert((clGetProgramBuildInfo(kernel_program_396.get(), device.get(), CL_PROGRAM_BUILD_LOG, logsize, log, NULL) == CL_SUCCESS)); 
		std::cout << log << std::endl; 
		exit(1); 
	}
	kernel_396 = cl::Kernel(kernel_program_396, "KERNEL"); 
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
		double cpu_time_ms = cpu_time_in_ms(cpu_clock_start_397, cpu_clock_end_397);
		double gpu_time_ms = gpu_time_in_ms(event_397);
		double diff_pc = diff_percent(cpu_time_ms, gpu_time_ms);
		std::cout<<"ToGPU_397"<<", "<<cpu_time_ms<<", "<<gpu_time_ms<<", "<<diff_pc<<std::endl; 
	}
	{
		double cpu_time_ms = cpu_time_in_ms(cpu_clock_start_396, cpu_clock_end_396);
		double gpu_time_ms = gpu_time_in_ms(event_396);
		double diff_pc = diff_percent(cpu_time_ms, gpu_time_ms);
		std::cout<<"OclFunCall_396"<<", "<<cpu_time_ms<<", "<<gpu_time_ms<<", "<<diff_pc<<std::endl; 
	}
	{
		double cpu_time_ms = cpu_time_in_ms(cpu_clock_start_391, cpu_clock_end_391);
		double gpu_time_ms = gpu_time_in_ms(event_391);
		double diff_pc = diff_percent(cpu_time_ms, gpu_time_ms);
		std::cout<<"ToGPU_391"<<", "<<cpu_time_ms<<", "<<gpu_time_ms<<", "<<diff_pc<<std::endl; 
	}
	{
		double cpu_time_ms = cpu_time_in_ms(cpu_clock_start_393, cpu_clock_end_393);
		double gpu_time_ms = gpu_time_in_ms(event_393);
		double diff_pc = diff_percent(cpu_time_ms, gpu_time_ms);
		std::cout<<"ToGPU_393"<<", "<<cpu_time_ms<<", "<<gpu_time_ms<<", "<<diff_pc<<std::endl; 
	}
	{
		double cpu_time_ms = cpu_time_in_ms(cpu_clock_start_395, cpu_clock_end_395);
		double gpu_time_ms = gpu_time_in_ms(event_395);
		double diff_pc = diff_percent(cpu_time_ms, gpu_time_ms);
		std::cout<<"ToGPU_395"<<", "<<cpu_time_ms<<", "<<gpu_time_ms<<", "<<diff_pc<<std::endl; 
	}
}
void post_execute(){
	print_clock(); 
}

namespace lift {; 

	void execute(float * v_initial_param_384_99, float * v_initial_param_385_100, float * v_initial_param_386_101, float * & v_user_func_397_106, int v_M_1, int v_K_3, int v_N_0, int v_O_2){
		// Allocate memory for output pointers
		cl::Buffer v_user_func_395_104(context, CL_MEM_READ_WRITE, ((v_N_0 * v_O_2) * sizeof(float)));
		cl::Buffer v_user_func_396_105(context, CL_MEM_READ_WRITE, ((v_M_1 * v_O_2) * sizeof(float)));
		cl::Buffer v__73(context, CL_MEM_READ_WRITE, ((v_N_0 * v_M_1) * sizeof(float)));
		cl::Buffer v_user_func_391_102(context, CL_MEM_READ_WRITE, ((v_M_1 * v_K_3) * sizeof(float)));
		cl::Buffer v_user_func_393_103(context, CL_MEM_READ_WRITE, ((v_N_0 * v_K_3) * sizeof(float)));
		v_user_func_397_106 = reinterpret_cast<float *>(malloc(((v_M_1 * v_O_2) * sizeof(float)))); 
		cpu_clock_start_391 = std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::system_clock::now().time_since_epoch()); 
		lift_queue.enqueueWriteBuffer(v_user_func_391_102, CL_TRUE, 0, ((v_M_1 * v_K_3) * sizeof(float)), v_initial_param_384_99, NULL, (&event_391)); 
		assert(lift_queue.finish() == CL_SUCCESS); 
		cpu_clock_end_391 = std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::system_clock::now().time_since_epoch()); 
		cpu_clock_start_393 = std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::system_clock::now().time_since_epoch()); 
		lift_queue.enqueueWriteBuffer(v_user_func_393_103, CL_TRUE, 0, ((v_N_0 * v_K_3) * sizeof(float)), v_initial_param_385_100, NULL, (&event_393)); 
		assert(lift_queue.finish() == CL_SUCCESS); 
		cpu_clock_end_393 = std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::system_clock::now().time_since_epoch()); 
		cpu_clock_start_395 = std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::system_clock::now().time_since_epoch()); 
		lift_queue.enqueueWriteBuffer(v_user_func_395_104, CL_TRUE, 0, ((v_N_0 * v_O_2) * sizeof(float)), v_initial_param_386_101, NULL, (&event_395)); 
		assert(lift_queue.finish() == CL_SUCCESS); 
		cpu_clock_end_395 = std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::system_clock::now().time_since_epoch()); 
		kernel_396.setArg(0, v_user_func_391_102); 
		kernel_396.setArg(1, v_user_func_393_103); 
		kernel_396.setArg(2, v_user_func_395_104); 
		kernel_396.setArg(3, v_user_func_396_105); 
		kernel_396.setArg(4, v__73); 
		kernel_396.setArg(5, v_K_3); 
		kernel_396.setArg(6, v_M_1); 
		kernel_396.setArg(7, v_N_0); 
		kernel_396.setArg(8, v_O_2); 
		cpu_clock_start_396 = std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::system_clock::now().time_since_epoch()); 
		lift_queue.enqueueNDRangeKernel(kernel_396, cl::NullRange, cl::NDRange(v_O_2,v_M_1,1), cl::NDRange(v_O_2,v_M_1,1), NULL, (&event_396)); 
		assert(lift_queue.finish() == CL_SUCCESS); 
		cpu_clock_end_396 = std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::system_clock::now().time_since_epoch()); 
		cpu_clock_start_397 = std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::system_clock::now().time_since_epoch()); 
		lift_queue.enqueueReadBuffer(v_user_func_396_105, CL_TRUE, 0, ((v_M_1 * v_O_2) * sizeof(float)), v_user_func_397_106, NULL, (&event_397)); 
		assert(lift_queue.finish() == CL_SUCCESS); 
		cpu_clock_end_397 = std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::system_clock::now().time_since_epoch()); 
		post_execute(); 
	}
}; 
