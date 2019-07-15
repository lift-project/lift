#include <bits/stdc++.h>
#include "libsequential_conv_naums_version.cpp"

using namespace std;

int main(int argc, char *argv[])
{


	constexpr unsigned int kernel_h = 4;
	constexpr unsigned int kernel_w = 5;

	constexpr unsigned int input_xdim = 13;
	constexpr unsigned int input_ydim = 9;
	constexpr unsigned int in_channels = 3;

	constexpr unsigned int out_channels = 10;

	constexpr unsigned int K_size = kernel_w * kernel_h * in_channels * out_channels;
	constexpr unsigned int B_size = out_channels;
	constexpr unsigned int X_size = input_xdim * input_ydim * in_channels;

	vector<float> K(K_size,1.0f), B(B_size, 1.0f), X(X_size, 1.0f);
	float *out = nullptr;

	lift::execute(K.data(), B.data(), X.data(), out);

	/* copy(out,out+output_N*output_N*output_N, ostream_iterator<float>(cout, " ")); */
	/* std::cout << std::endl; */

	return 0;
}
