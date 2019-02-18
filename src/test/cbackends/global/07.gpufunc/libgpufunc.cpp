
#include <bits/stdc++.h>

using namespace std;

    
#include <CL/cl.hpp>


std::string readFile(const char *filename){
	std::ifstream in(filename, std::ios::in);

	if (in.fail())
	{
		std::cerr << "Error reading file " << filename << std::endl;
		exit(1);
	}

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
std::string kernel_string_80;
cl::Program::Sources kernel_source_80;
cl::Program kernel_program_80;
cl::Kernel kernel_80;
void lift_init(){
    kernel_string_80 = readFile("./kernel_80.cl"); 
    kernel_source_80 = cl::Program::Sources(1, std::make_pair(kernel_string_80.c_str(), kernel_string_80.length())); 
    kernel_program_80 = cl::Program(context, kernel_source_80); 
    if (kernel_program_80.build({ device }) != CL_SUCCESS){
        std::cerr<<"kernel build error"<<std::endl; exit(1);; 
    }
    kernel_80 = cl::Kernel(kernel_program_80, "KERNEL"); 
}


void execute(float * v_initial_param_46_18, float * & v_user_func_81_21, int v_N_0){
    // Allocate memory for output pointers
    cl::Buffer v_user_func_80_20(context, CL_MEM_READ_WRITE, (v_N_0 * sizeof(float)));
    v_user_func_81_21 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    cl::Buffer v_user_func_79_19(context, CL_MEM_READ_WRITE, (v_N_0 * sizeof(float)));
    cl::Event event_79;
    lift_queue.enqueueWriteBuffer(v_user_func_79_19, CL_TRUE, 0, (v_N_0 * sizeof(float)), v_initial_param_46_18, NULL, (&event_79)); 
    kernel_80.setArg(0, v_user_func_79_19);
    kernel_80.setArg(1, v_user_func_80_20);
    kernel_80.setArg(2, v_N_0);
    cl::Event event_80;
    lift_queue.enqueueNDRangeKernel(kernel_80, cl::NullRange, cl::NDRange(1,1,1), cl::NDRange(1,1,1), NULL, (&event_80)); 
    cl::Event event_81;
    lift_queue.enqueueReadBuffer(v_user_func_80_20, CL_TRUE, 0, (v_N_0 * sizeof(float)), v_user_func_81_21, NULL, (&event_81)); 
}
