#include <bits/stdc++.h>
#include "./libconv3d_host.cpp"

using namespace std;

int main(int argc, char *argv[])
{

	int const N = 8;
	int const output_N = 3;
	int const feature_size=6;
	vector<float> in(N*N*N,1.0f), weights(feature_size*feature_size*feature_size, 2.0f);
	float *out = nullptr;

        /* std::cout << "output size = " << (-8 + (-6 * (int)pow((float)N, 2)) + (12 * N) + (int)pow((float)N, 3)) << std::endl; */

	lift::execute(in.data(), weights.data(), out);

	copy(out,out+output_N*output_N*output_N, ostream_iterator<float>(cout, " "));
	std::cout << std::endl;
	
	return 0;
}
