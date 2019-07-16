#include <bits/stdc++.h>
#include "libsequential_conv_naums_version.cpp"

using namespace std;

int main(int argc, char *argv[])
{


	constexpr unsigned int kernel_h = 3; //done
	constexpr unsigned int kernel_w = 3; //done

	constexpr unsigned int input_xdim = 8; //done
	constexpr unsigned int input_ydim = 2;  //done
	constexpr unsigned int in_channels = 2; //done

	constexpr unsigned int out_channels = 3; //done

	constexpr unsigned int K_size = kernel_w * kernel_h * in_channels * out_channels;
	constexpr unsigned int B_size = out_channels;
	//X is 5 dimensional
	constexpr unsigned int X_size = input_xdim * input_ydim * in_channels;

	vector<float> K{
		1.0f, 0.0f, 1.0f, 0.0f, 1.0f, 0.0f,
		3.0f, 0.0f, 3.0f, 0.0f, 3.0f, 0.0f,
		5.0f, 0.0f, 5.0f, 0.0f, 5.0f, 0.0f,
		7.0f, 0.0f, 7.0f, 0.0f, 7.0f, 0.0f,
		9.0f, 0.0f, 9.0f, 0.0f, 9.0f, 0.0f,
		11.0f, 0.0f, 11.0f, 0.0f, 11.0f, 0.0f,
		13.0f, 0.0f, 13.0f, 0.0f, 13.0f, 0.0f,
		15.0f, 0.0f, 15.0f, 0.0f, 15.0f, 0.0f,
		17.0f, 0.0f, 17.0f, 0.0f, 17.0f, 0.0f
	};
	assert(K.size() == K_size);
	vector<float> B{
		0.0f, 1.0f, 2.0f
	};
	assert(N.size() == B_size);
	vector<float> X{
		0.0f, 0.0f,   1.0f, 1.0f,   2.0f, 2.0f,   3.0f, 3.0f,
		4.0f, 4.0f,   5.0f, 5.0f,   6.0f, 6.0f,  7.0f, 7.0f,
		8.0f, 8.0f,   9.0f, 9.0f,   10.0f, 10.0f, 11.0f, 11.0f,
		12.0f, 12.0f, 13.0f, 13.0f, 14.0f, 14.0f, 15.0f, 15.0f,
		16.0f, 16.0f, 17.0f, 17.0f, 18.0f, 18.0f, 19.0f, 19.0f,
		20.0f, 20.0f, 21.0f, 21.0f, 22.0f, 22.0f, 23.0f, 23.0f,
		24.0f, 24.0f, 25.0f, 25.0f, 26.0f, 26.0f, 27.0f, 27.0f,
		28.0f, 28.0f, 29.0f, 29.0f, 30.0f, 30.0f, 31.0f, 31.0f,
		32.0f, 32.0f, 33.0f, 33.0f, 34.0f, 34.0f, 35.0f, 35.0f,
		36.0f, 36.0f, 37.0f, 37.0f, 38.0f, 38.0f, 39.0f, 39.0f,
		40.0f, 40.0f, 41.0f, 41.0f, 42.0f, 42.0f, 43.0f, 43.0f,
		44.0f, 44.0f, 45.0f, 45.0f, 46.0f, 46.0f, 47.0f, 47.0f,
		48.0f, 48.0f, 49.0f, 49.0f, 50.0f, 50.0f, 51.0f, 51.0f,
		52.0f, 52.0f, 53.0f, 53.0f, 54.0f, 54.0f, 55.0f, 55.0f,
		56.0f, 56.0f, 57.0f, 57.0f, 58.0f, 58.0f, 59.0f, 59.0f,
		60.0f, 60.0f, 61.0f, 61.0f, 62.0f, 62.0f, 63.0f, 63.0f,
		0.0f, 0.0f,   1.0f, 1.0f,   2.0f, 2.0f,   3.0f, 3.0f,
		4.0f, 4.0f,   5.0f, 5.0f,   6.0f, 6.0f,  7.0f, 7.0f,
		8.0f, 8.0f,   9.0f, 9.0f,   10.0f, 10.0f, 11.0f, 11.0f,
		12.0f, 12.0f, 13.0f, 13.0f, 14.0f, 14.0f, 15.0f, 15.0f,
		16.0f, 16.0f, 17.0f, 17.0f, 18.0f, 18.0f, 19.0f, 19.0f,
		20.0f, 20.0f, 21.0f, 21.0f, 22.0f, 22.0f, 23.0f, 23.0f,
		24.0f, 24.0f, 25.0f, 25.0f, 26.0f, 26.0f, 27.0f, 27.0f,
		28.0f, 28.0f, 29.0f, 29.0f, 30.0f, 30.0f, 31.0f, 31.0f,
		32.0f, 32.0f, 33.0f, 33.0f, 34.0f, 34.0f, 35.0f, 35.0f,
		36.0f, 36.0f, 37.0f, 37.0f, 38.0f, 38.0f, 39.0f, 39.0f,
		40.0f, 40.0f, 41.0f, 41.0f, 42.0f, 42.0f, 43.0f, 43.0f,
		44.0f, 44.0f, 45.0f, 45.0f, 46.0f, 46.0f, 47.0f, 47.0f,
		48.0f, 48.0f, 49.0f, 49.0f, 50.0f, 50.0f, 51.0f, 51.0f,
		52.0f, 52.0f, 53.0f, 53.0f, 54.0f, 54.0f, 55.0f, 55.0f,
		56.0f, 56.0f, 57.0f, 57.0f, 58.0f, 58.0f, 59.0f, 59.0f,
		60.0f, 60.0f, 61.0f, 61.0f, 62.0f, 62.0f, 63.0f, 63.0f
	};
	assert(X.size() == X_size);




	float *out = nullptr;

	lift::execute(K.data(), B.data(), X.data(), out);

	/* copy(out,out+output_N*output_N*output_N, ostream_iterator<float>(cout, " ")); */
	/* std::cout << std::endl; */

	return 0;
}
