#define CL_HPP_TARGET_OPENCL_VERSION 200
#define CL_TARGET_OPENCL_VERSION 200
#include <bits/stdc++.h>

#include <libconv.cpp>

#include <golden_conv_impl.cpp>

using namespace std;

int main(int argc, char *argv[])
{
	assert(argc == 2);
	const std::string kernel_path(argv[1]);

	constexpr int padded_input_width = INPUT_WIDTH + PAD_FUNC_X * 2;
	constexpr int padded_input_height = INPUT_HEIGHT + PAD_FUNC_Y * 2;

	constexpr int in_size = INPUT_CHANNELS * padded_input_width * padded_input_height; //861184;
	constexpr int w_size = INPUT_CHANNELS * KERNEL_WIDTH * KERNEL_HEIGHT * NUM_KERNELS; //73728;
	constexpr int out_size = NUM_KERNELS *
	                           ((padded_input_width - (KERNEL_WIDTH - KERNEL_STRIDE_X)) / KERNEL_STRIDE_X) *
	                           ((padded_input_height - (KERNEL_HEIGHT - KERNEL_STRIDE_Y)) / KERNEL_STRIDE_Y); //1663488;

	
	float *in = (float *) malloc( in_size * sizeof(float));
	float *w = (float *) malloc( w_size * sizeof(float));
	float *out = nullptr;
#ifdef VERIFY_CORRECTNESS
	float *out_golden = reinterpret_cast<float *>(malloc((out_size * sizeof(float)))); 
	std::fill(out_golden, out_golden + out_size, 0);
#endif


	std::srand(std::time(nullptr));
	generate(in, in + in_size, [](){ return (rand()%100000)/100000.0f;} );
	generate(w, w + w_size, [](){ return (rand()%100000)/100000.0f;} );

	lift_init(kernel_path);

    for (int trial = 0; trial < TRIALS; trial++) {
        std::cout << "Trial " << trial << ":" << std::endl;
	    lift::execute(in, w, out);
    }
	assert( out != nullptr);

#ifdef VERIFY_CORRECTNESS
	//non-coalesced sequential cpu
	golden_convolution(in, w, out_golden,
	    INPUT_CHANNELS, padded_input_height, padded_input_width,
	    KERNEL_HEIGHT, KERNEL_WIDTH, KERNEL_STRIDE_Y, KERNEL_STRIDE_X,
	    NUM_KERNELS);//64, 116, 116, 3, 3, 128);

	
	for (int i = 0; i < out_size; ++i) {
		if( abs(out[i]-out_golden[i])/out_golden[i] > 0.00001){
			std::cerr << "Error at index " << i << std::endl;
			std::cerr << "golden: " << out_golden[i] << std::endl;
			std::cerr << "computed: " << out[i] << std::endl;
			std::cerr << "diff: " << abs(out[i]-out_golden[i])/out_golden[i] << std::endl;
			exit(1);
		}
//        std::cerr << "g=" << out_golden[i] << " a=" << out[i] << std::endl;
	}

	std::cout << "VERIFICATION: SUCCESS" << std::endl;
#endif

	return 0;
}
