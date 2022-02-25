#include <bits/stdc++.h>
#define CL_HPP_TARGET_OPENCL_VERSION 200

using namespace std;


//data layout, from outputmost to innermost
// image: #in_channels x height x width
// kernel: #out_channels x in_channels x height x width
// output: #out_channels x output_height x output_width
void golden_convolution(float const *input,
		float const *weights,
//		float const *biases,
		float *output,
		int const input_channels,
		int const input_height,
		int const input_width,
		int const kernel_height,
		int const kernel_width,
		int const num_kernels) {


	int const output_width = input_width - kernel_width + 1;
	int const output_height = input_height - kernel_height + 1;
	int x,y, input_idx, weight_idx, input_offset, weight_offset;


	for(int kx = 0; kx < num_kernels; ++kx){
		for(int wy=0; wy<input_height-kernel_height+1;wy++){
			for(int wx=0; wx<input_width-kernel_width+1;wx++) {
				float dot_product = 0; //biases[kx];
				for(int icx = 0; icx < input_channels; ++icx){
					for(int ey=0;ey<kernel_height;ey++) {
						for(int ex=0;ex<kernel_width;ex++) {
							x = wx + ex;
							y = wy + ey;
							input_idx = y * input_width + x;
							weight_idx = ey * kernel_width + ex;
							input_offset = icx * input_height * input_width;
							weight_offset =
								kx * input_channels * kernel_height * kernel_width
								+ icx * kernel_height * kernel_width ;



							dot_product += input[ input_offset + input_idx ]
								* weights[ weight_offset + weight_idx ];
						}
					}
				}
				output[ kx * output_height * output_width + wy * output_width + wx  ] = dot_product;
			}
		}

	}



}


#ifdef TEST
#include "./libconv.cpp"
extern void execute(float const *input, float const *weights, /*float const *biases, */float * output);
/* extern void execute(float const *input, float const *weights, float const *biases, float * output, float *temp); */
#endif

int main(int argc, char *argv[])
{

#ifdef TEST
	assert(argc == 2);
	const std::string kernel_path(argv[1]);
	/* std::cout << kernel_path << std::endl; */
#endif

    constexpr int pad_per_config = 1;
    constexpr int pad_optimisational = 1;
	constexpr int input_channels = 64;
	constexpr int input_height = 112;
	constexpr int input_width = 112;
	constexpr int input_height_padded = input_height + pad_per_config * 2 + pad_optimisational * 2;
	constexpr int input_width_padded = input_width + pad_per_config * 2 + pad_optimisational * 2;
	constexpr int kernel_height = 3;
	constexpr int kernel_width = 3;
	constexpr int num_kernels = 128;

	constexpr int kernel_stride_height = 1;
	constexpr int kernel_stride_width = 1;

	const int kernelCacheSize = 1;
	const int tileWidth = 1;
	const int tileHeight = 1;

	constexpr int input_length = input_channels * input_height_padded * input_width_padded;
	constexpr int kernel_length = input_channels * kernel_height * kernel_width * num_kernels;
//	constexpr int bias_lengh = num_kernels;
	constexpr int output_length = num_kernels * (input_height_padded - kernel_height + 1) * (input_width_padded - kernel_width + 1) ;

	//vector<float> input(input_length), kernel(kernel_length), biases(bias_lengh), output(output_length, 0);
	vector<float> input(input_length), kernel(kernel_length); //, biases(bias_lengh);
#ifndef TEST
	vector<float> output(output_length, 0);
#else
	float * output = nullptr;
#endif

	/* vector<float> output2(output_length, 0); */

//	iota(input.begin(), input.end(), 1);
//	iota(kernel.begin(), kernel.end(), 0);
//	iota(biases.begin(), biases.end(), 0);
    string random_x_file_path = "C:\\Users\\Naums\\cases-experiments20\\random_x_vgg_conv1_double.txt";
    FILE* random_x_file = fopen(random_x_file_path.c_str(), "r");
    if (random_x_file == NULL) {
        std::cerr << "Couldn't open the sample input data file " << random_x_file_path << "\nExiting.\n";
        return(1);
    }

    for(int i = 0; i < input_length; i++)
        fscanf(random_x_file, "%f", input.data() + i);

//    random_x_file.clear();
//    copy(start_x, end_x, kernel.begin());
    int offset = 10;
    rewind(random_x_file);
    for(int i = 0; i < offset; i++)
        fscanf(random_x_file, "%f", kernel.data());

    for(int i = 0; i < kernel_length; i++)
        fscanf(random_x_file, "%f", kernel.data() + i);

#ifndef PRINT_ALL
#define PRINT_ALL
#endif

#ifdef PRINT_ALL
	std::cout << "input:" << std::endl;
	copy(input.begin(), input.end(),   ostream_iterator<float>(cout, " "));
	std::cout << std::endl;
	std::cout << std::endl;

	std::cout << "kernel:" << std::endl;
	copy(kernel.begin(), kernel.end(), ostream_iterator<float>(cout, " "));
	std::cout << std::endl;
	std::cout << std::endl;

//	std::cout << "bias:" << std::endl;
//	copy(biases.begin(), biases.end(), ostream_iterator<float>(cout, " "));
//	std::cout << std::endl;
//	std::cout << std::endl;

//	std::cout << "output:" << std::endl;
//	copy(output.begin(), output.end(), ostream_iterator<float>(cout, " "));
//	std::cout << std::endl;
//	std::cout << std::endl;
#endif

#ifndef TEST
	golden_convolution(input.data(), kernel.data(), /*biases.data(), */output.data(), input_channels, input_height_padded, input_width_padded, kernel_height, kernel_width, num_kernels);
#else

    int n_windows_x = (input_width - kernel_width) / kernel_stride_width + 1;
	int n_windows_y = (input_height - kernel_height)/kernel_stride_height + 1;
	int n_tiles_x = n_windows_x / tileWidth;
	int n_tiles_y = n_windows_y / tileHeight;
//	lift_global_0 = (input_width_padded - kernel_width )/kernel_stride_width + 1;
//	lift_global_1 = (input_height_padded - kernel_height)/kernel_stride_height + 1;
//	lift_global_2 = num_kernels;
    lift_global_0 = n_tiles_x; //(input_width - kernel_width )/kernel_stride_width + 1;
    lift_global_1 = n_tiles_y; //(input_height - kernel_height)/kernel_stride_height + 1;
    lift_global_2 = num_kernels / kernelCacheSize;
	lift_init(kernel_path);
	lift::execute(input.data(), kernel.data(), /*biases.data(), */output);
	/* execute(input.data(), kernel.data(), biases.data(), output.data(), output2.data()); */
#endif

#ifdef PRINT_ALL
	std::cout << "output:" << std::endl;
#endif

#ifndef TEST
	copy(output.begin(), output.end(), ostream_iterator<float>(cout << std::setprecision(5), " "));
#else
	copy(output, output + output_length, ostream_iterator<float>(cout << std::setprecision(5), " "));
#endif
	std::cout << std::endl;


#ifdef PRINT_ALL
	std::cout << std::endl;
#endif

	return 0;
}