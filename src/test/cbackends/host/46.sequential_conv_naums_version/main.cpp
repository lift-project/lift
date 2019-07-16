#include <bits/stdc++.h>
#include "libsequential_conv_naums_version.cpp"

using namespace std;

int main(int argc, char *argv[])
{


	constexpr unsigned int kernel_h = 3; //done
	constexpr unsigned int kernel_w = 3; //done

	constexpr unsigned int input_xdim = 8; //done
	constexpr unsigned int input_ydim = 8;  //done
	constexpr unsigned int in_channels = 2; //done

	constexpr unsigned int out_channels = 3; //done

	constexpr unsigned int K_size = kernel_w * kernel_h * in_channels * out_channels;
	constexpr unsigned int B_size = out_channels;
	constexpr unsigned int X_size = input_xdim * input_ydim * in_channels;
	constexpr unsigned int O_size = ( input_xdim - kernel_w + 1 ) * ( input_ydim - kernel_h + 1 ) * out_channels ;

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
	assert(B.size() == B_size);
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
		60.0f, 60.0f, 61.0f, 61.0f, 62.0f, 62.0f, 63.0f, 63.0f

	};
	/* std::cout << X.size() << " ==? " << X_size << std::endl; */
	assert(X.size() == X_size);

	vector<float> O_golden{
		1029, 1029+1, 1029+2, 1110, 1110+1, 1110+2, 1191, 1191+1, 1191+2,
		1272, 1272+1, 1272+2, 1353, 1353+1, 1353+2, 1434, 1434+1, 1434+2,
		1677, 1677+1, 1677+2, 1758, 1758+1, 1758+2, 1839, 1839+1, 1839+2,
		1920, 1920+1, 1920+2, 2001, 2001+1, 2001+2, 2082, 2082+1, 2082+2,
		2325, 2325+1, 2325+2, 2406, 2406+1, 2406+2, 2487, 2487+1, 2487+2,
		2568, 2568+1, 2568+2, 2649, 2649+1, 2649+2, 2730, 2730+1, 2730+2,
		2973, 2973+1, 2973+2, 3054, 3054+1, 3054+2, 3135, 3135+1, 3135+2,
		3216, 3216+1, 3216+2, 3297, 3297+1, 3297+2, 3378, 3378+1, 3378+2,
		3621, 3621+1, 3621+2, 3702, 3702+1, 3702+2, 3783, 3783+1, 3783+2,
		3864, 3864+1, 3864+2, 3945, 3945+1, 3945+2, 4026, 4026+1, 4026+2,
		4269, 4269+1, 4269+2, 4350, 4350+1, 4350+2, 4431, 4431+1, 4431+2,
		4512, 4512+1, 4512+2, 4593, 4593+1, 4593+2, 4674, 4674+1, 4674+2
	};

	assert( O_golden.size() == O_size );

	float *out = nullptr;

	//TODO: the layout is not the same, and the dimensions compiled directly is not the same as well,
	//so this will definitely not produce the correct results.
	//fix: first need to pass dimensions, second, transpose the dimensions correctly if you want to test it now.
	lift::execute(K.data(), B.data(), X.data(), out);

	copy(out,out+O_size, ostream_iterator<float>(cout, " "));
	std::cout << std::endl;

	for(auto i = 0; i< O_size; ++i) assert(out[i] == O_golden[i]);

	return 0;
}
