#include <bits/stdc++.h>

using namespace std;


void golden_convolution(float const *input,
		float const *weights,
		float const *biases,
		float *output,
		int const input_channels,
		int const input_height,
		int const input_width,
		int const kernel_height,
		int const kernel_width,
		int const num_kernels) {

	//need two kinds of coordinates to locate a computation
	//1) sliding window coordinates (x,y)
	//2) element coordinates (x,y) in one sliding window
	//for each element, there are "input_channels" floats

	//output shape: height, width, kernels

	int const output_width = input_width - kernel_width + 1;
	int const output_height = input_height - kernel_height + 1;

	for(int wy=0; wy<input_height-kernel_height+1;wy++)
		for(int wx=0; wx<input_width-kernel_width+1;wx++) 
			for(int kx = 0; kx < num_kernels; ++kx) {

				int dot_product = 0;
				int x,y, input_idx, weight_idx, weight_offset;
#ifdef PRINT_ALL
				std::cout << "----" << std::endl;
#endif
				for(int ey=0;ey<kernel_height;ey++)
					for(int ex=0;ex<kernel_width;ex++) {
						x = wx + ex;
						y = wy + ey;
						input_idx = y*input_width + x;
						weight_idx = ey * kernel_width + ex;
						weight_offset = kernel_height * kernel_width * input_channels * kx;
						for(int icx = 0; icx < input_channels; ++icx)  {
#ifdef PRINT_ALL
							std::cout << input_idx * input_channels + icx << " * " << weight_offset + weight_idx * input_channels+icx << ": ";
							std::cout << input[input_idx * input_channels + icx] << " * " << weights[weight_offset + weight_idx * input_channels+icx] << std::endl;
#endif
							dot_product += input[input_idx * input_channels + icx]*weights[weight_offset + weight_idx * input_channels+icx];
						}
					}
#ifdef PRINT_ALL
				std::cout << "dot_product = " << dot_product << std::endl;
				std::cout << std::endl <<   wy * output_width * num_kernels + wx * num_kernels + kx << std::endl;
				std::cout << "----" << std::endl;
#endif

				/* output[ kx * output_width * output_height + wy * output_width + wx ] = dot_product + biases[kx]; */
				output[  wy * output_width * num_kernels + wx * num_kernels + kx ] = dot_product + biases[kx];
			}


}

#ifdef TEST
#include "./libconv.cpp"
extern void execute(float const *input, float const *weights, float const *biases, float * output);
/* extern void execute(float const *input, float const *weights, float const *biases, float * output, float *temp); */
#endif

int main(int argc, char *argv[])
{

#ifdef TEST
	assert(argc == 2);
	const std::string kernel_path(argv[1]);
	/* std::cout << kernel_path << std::endl; */
#endif

	constexpr int input_channels = 3;
	constexpr int input_height = 9;
	constexpr int input_width = 9;
	constexpr int kernel_height = 2;
	constexpr int kernel_width = 4;
	constexpr int num_kernels = 4;

	constexpr int kernel_stride_height = 1;
	constexpr int kernel_stride_width = 1;

	constexpr int input_length = input_channels * input_height * input_width;
	constexpr int kernel_length = input_channels * kernel_height * kernel_width * num_kernels;
	constexpr int bias_lengh = num_kernels;
	constexpr int output_length = num_kernels * (input_height - kernel_height + 1) * (input_width - kernel_width + 1) ;

	//vector<float> input(input_length), kernel(kernel_length), biases(bias_lengh), output(output_length, 0);
	vector<float> input(input_length), kernel(kernel_length), biases(bias_lengh);
#ifndef TEST
	vector<float> output(output_length, 0);
#else
	float * output = nullptr;
#endif
	
	/* vector<float> output2(output_length, 0); */

	iota(input.begin(), input.end(), 1);
	iota(kernel.begin(), kernel.end(), 0);
	iota(biases.begin(), biases.end(), 0);

#ifdef PRINT_ALL
	std::cout << "input:" << std::endl;
	copy(input.begin(), input.end(),   ostream_iterator<float>(cout, " "));
	std::cout << std::endl;
	std::cout << std::endl;

	std::cout << "kernel:" << std::endl;
	copy(kernel.begin(), kernel.end(), ostream_iterator<float>(cout, " "));
	std::cout << std::endl;
	std::cout << std::endl;

	std::cout << "bias:" << std::endl;
	copy(biases.begin(), biases.end(), ostream_iterator<float>(cout, " "));
	std::cout << std::endl;
	std::cout << std::endl;

	std::cout << "output:" << std::endl;
	copy(output.begin(), output.end(), ostream_iterator<float>(cout, " "));
	std::cout << std::endl;
	std::cout << std::endl;
#endif

#ifndef TEST
	golden_convolution(input.data(), kernel.data(), biases.data(), output.data(), input_channels, input_height, input_width, kernel_height, kernel_width, num_kernels);
#else

	lift_global_0 = num_kernels;
	lift_global_1 = (input_width - kernel_width )/kernel_stride_width + 1;
	lift_global_2 = (input_height - kernel_height)/kernel_stride_height + 1;
	lift_init(kernel_path);
	lift::execute(input.data(), kernel.data(), biases.data(), output);
	/* execute(input.data(), kernel.data(), biases.data(), output.data(), output2.data()); */
#endif

#ifdef PRINT_ALL
	std::cout << "output:" << std::endl;
#endif

#ifndef TEST
	copy(output.begin(), output.end(), ostream_iterator<float>(cout, " "));
#else
	copy(output, output + output_length, ostream_iterator<float>(cout, " "));
#endif
	std::cout << std::endl;


#ifdef PRINT_ALL
	std::cout << std::endl;
#endif

	return 0;
}