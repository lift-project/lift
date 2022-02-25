
#include <bits/stdc++.h>

using namespace std;

    

#include <iostream>
#include <CL/cl2.hpp>
#include <fstream>

std::string kernel_string_165;
cl::Program::Sources kernel_source_165;
cl::Program kernel_program_165;
cl::Kernel kernel_165;
std::chrono::milliseconds cpu_clock_start_165;
std::chrono::milliseconds cpu_clock_end_165;
cl::Event event_165;
std::chrono::milliseconds cpu_clock_start_162;
std::chrono::milliseconds cpu_clock_end_162;
cl::Event event_162;
std::chrono::milliseconds cpu_clock_start_164;
std::chrono::milliseconds cpu_clock_end_164;
cl::Event event_164;
std::chrono::milliseconds cpu_clock_start_166;
std::chrono::milliseconds cpu_clock_end_166;
cl::Event event_166;


void init_kernel() {
    kernel_string_165 = readFile("kernel_165.cl"); 
    kernel_source_165 = cl::Program::Sources(1, {kernel_string_165.c_str(), kernel_string_165.length()}); 
    kernel_program_165 = cl::Program(context, kernel_source_165); 
    /* const char *option = "-cl-opt-disable"; */
    const char *option = NULL;
    /* int ret = kernel_program_165.build({ device }, option); */
    /* std::cout << "build code = " << ret << std::endl; */
    if ((kernel_program_165.build({ device }, option) != CL_SUCCESS)){
        std::cerr << "kernel build error" << std::endl; 
        char* log; 
        size_t logsize; 
        assert((clGetProgramBuildInfo(kernel_program_165.get(), device.get(), CL_PROGRAM_BUILD_LOG, 0, NULL, &logsize) == CL_SUCCESS)); 
        log = (char*)malloc(sizeof(char) * logsize); 
        assert((clGetProgramBuildInfo(kernel_program_165.get(), device.get(), CL_PROGRAM_BUILD_LOG, logsize, log, NULL) == CL_SUCCESS)); 
        std::cout << log << std::endl; 
        exit(1); 
    }
    kernel_165 = cl::Kernel(kernel_program_165, "KERNEL"); 
}
    

void print_clock1(){
    std::cout<<"func_name, cpu_time_ms, gpu_time_ms, diff_percentage"<<std::endl; 
    {
        double cpu_time_ms = cpu_time_in_ms(cpu_clock_start_166, cpu_clock_end_166);
        double gpu_time_ms = gpu_time_in_ms(event_166);
        double diff_pc = diff_percent(cpu_time_ms, gpu_time_ms);
        std::cout<<"ToGPU_166"<<", "<<cpu_time_ms<<", "<<gpu_time_ms<<", "<<diff_pc<<std::endl; 
    }
    {
        double cpu_time_ms = cpu_time_in_ms(cpu_clock_start_165, cpu_clock_end_165);
        double gpu_time_ms = gpu_time_in_ms(event_165);
        double diff_pc = diff_percent(cpu_time_ms, gpu_time_ms);
        std::cout<<"OclFunCall_165"<<", "<<cpu_time_ms<<", "<<gpu_time_ms<<", "<<diff_pc<<std::endl; 
    }
    {
        double cpu_time_ms = cpu_time_in_ms(cpu_clock_start_162, cpu_clock_end_162);
        double gpu_time_ms = gpu_time_in_ms(event_162);
        double diff_pc = diff_percent(cpu_time_ms, gpu_time_ms);
        std::cout<<"ToGPU_162"<<", "<<cpu_time_ms<<", "<<gpu_time_ms<<", "<<diff_pc<<std::endl; 
    }
    {
        double cpu_time_ms = cpu_time_in_ms(cpu_clock_start_164, cpu_clock_end_164);
        double gpu_time_ms = gpu_time_in_ms(event_164);
        double diff_pc = diff_percent(cpu_time_ms, gpu_time_ms);
        std::cout<<"ToGPU_164"<<", "<<cpu_time_ms<<", "<<gpu_time_ms<<", "<<diff_pc<<std::endl; 
    }
}

      ; 


namespace lift {; 

void execute1(float * v_initial_param_156_38, float * v_initial_param_157_39, float * & v_user_func_166_43, int v_M_1, int v_K_3, int v_N_0){
    // Allocate memory for output pointers
    cl::Buffer v_user_func_162_40(context, CL_MEM_READ_WRITE, ((v_M_1 * v_K_3) * sizeof(float)));
    cl::Buffer v_user_func_164_41(context, CL_MEM_READ_WRITE, ((v_N_0 * v_K_3) * sizeof(float)));
    cl::Buffer v_user_func_165_42(context, CL_MEM_READ_WRITE, ((v_N_0 * v_M_1) * sizeof(float)));
    v_user_func_166_43 = reinterpret_cast<float *>(malloc(((v_N_0 * v_M_1) * sizeof(float)))); 
    cpu_clock_start_162 = std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::system_clock::now().time_since_epoch()); 
    lift_queue.enqueueWriteBuffer(v_user_func_162_40, CL_TRUE, 0, ((v_M_1 * v_K_3) * sizeof(float)), v_initial_param_156_38, NULL, (&event_162)); 
    assert(lift_queue.finish() == CL_SUCCESS); 
    cpu_clock_end_162 = std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::system_clock::now().time_since_epoch()); 
    cpu_clock_start_164 = std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::system_clock::now().time_since_epoch()); 
    lift_queue.enqueueWriteBuffer(v_user_func_164_41, CL_TRUE, 0, ((v_N_0 * v_K_3) * sizeof(float)), v_initial_param_157_39, NULL, (&event_164)); 
    assert(lift_queue.finish() == CL_SUCCESS); 
    cpu_clock_end_164 = std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::system_clock::now().time_since_epoch()); 
    kernel_165.setArg(0, v_user_func_162_40); 
    kernel_165.setArg(1, v_user_func_164_41); 
    kernel_165.setArg(2, v_user_func_165_42); 
    kernel_165.setArg(3, v_K_3); 
    kernel_165.setArg(4, v_M_1); 
    kernel_165.setArg(5, v_N_0); 
    cpu_clock_start_165 = std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::system_clock::now().time_since_epoch()); 
#if defined(FULL_THREADS)
#warning("use full threads")
    lift_queue.enqueueNDRangeKernel(kernel_165, cl::NullRange, cl::NDRange(v_M_1,v_N_0,1), cl::NDRange(v_M_1,v_N_0,1), NULL, (&event_165)); 
#elif defined(HALF_THREADS)
    lift_queue.enqueueNDRangeKernel(kernel_165, cl::NullRange, cl::NDRange(v_M_1/2, v_N_0/2,1,1), cl::NDRange(v_N_0/2,1,1), NULL, (&event_165)); 
#else
    lift_queue.enqueueNDRangeKernel(kernel_165, cl::NullRange, cl::NDRange(1,1,1), cl::NDRange(1,1,1), NULL, (&event_165)); 
#endif
    assert(lift_queue.finish() == CL_SUCCESS); 
    cpu_clock_end_165 = std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::system_clock::now().time_since_epoch()); 
    cpu_clock_start_166 = std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::system_clock::now().time_since_epoch()); 
    lift_queue.enqueueReadBuffer(v_user_func_165_42, CL_TRUE, 0, ((v_N_0 * v_M_1) * sizeof(float)), v_user_func_166_43, NULL, (&event_166)); 
    assert(lift_queue.finish() == CL_SUCCESS); 
    cpu_clock_end_166 = std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::system_clock::now().time_since_epoch()); 
    print_clock1(); 
}
}; 
