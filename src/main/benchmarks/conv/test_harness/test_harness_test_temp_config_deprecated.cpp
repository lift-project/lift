#include <bits/stdc++.h>
#define CL_HPP_TARGET_OPENCL_VERSION 200

using namespace std;


//data layout, from outputmost to innermost
// image: #in_channels x height x width
// kernel: #out_channels x in_channels x height x width
// output: #out_channels x output_height x output_width
void golden_test(float const *A,
		float *output,
		int const n,
		int const m,
		int const k) {


    for (int i = 0; i < k; i++) {
        for (int j = 0; j < n; j++) {
            for (int l = 0; l < m; l++) {
                // transposed
                output[l*n + j] += A[l + j*m + i*m*n];
            }
        }
    }
}


#ifdef TEST
#include "./libconv.cpp"
extern void execute(float const *A, float const *B, float * output);
#endif

int main(int argc, char *argv[])
{

#ifdef TEST
	assert(argc == 2);
	const std::string kernel_path(argv[1]);
	/* std::cout << kernel_path << std::endl; */
#endif

	constexpr int n = 7;
	constexpr int m = 5;
	constexpr int k = 3;

	//vector<float> input(input_length), kernel(kernel_length), biases(bias_lengh), output(output_length, 0);
	vector<float> A(n * m * k);
#ifndef TEST
	vector<float> output(n*m, 0);
#else
	float * output = nullptr;
#endif
//	vector<float> output(n * m, 0);

	iota(A.begin(), A.end(), 1);

	std::cout << "A:" << std::endl;
	copy(A.begin(), A.end(), ostream_iterator<float>(cout, " "));
	std::cout << "done." << std::endl;
	std::cout << std::endl;
	std::cout << std::endl;

//	std::cout << "B:" << std::endl;
//	copy(B.begin(), B.end(), ostream_iterator<float>(cout, " "));
//	std::cout << std::endl;
//	std::cout << std::endl;

//	std::cout << "output:" << std::endl;
//	copy(output.begin(), output.end(), ostream_iterator<float>(cout, " "));
//	std::cout << std::endl;
//	std::cout << std::endl;

#ifndef TEST
	golden_test(A.data(), output.data(), n, m, k);
#else

	lift_global_0 = 1;
	lift_global_1 = 1;
	lift_global_2 = 1;
	lift_init(kernel_path);
	lift::execute(A.data(), output);
#endif

	std::cout << "output:" << std::endl;
//	copy(output.begin(), output.end(), ostream_iterator<float>(cout << std::setprecision(4), " "));

#ifndef TEST
	copy(output.begin(), output.end(), ostream_iterator<float>(cout << std::setprecision(4), " "));
#else
	copy(output, output + n*m, ostream_iterator<float>(cout << std::setprecision(4), " "));
#endif
	std::cout << std::endl;
	std::cout << std::endl;

	return 0;
}