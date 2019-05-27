#include <bits/stdc++.h>
#include "./libpool_pool.cpp"

using namespace std;

int main(int argc, char *argv[])
{

	int const N = 6;
	int const output_N = 2;
	vector<float> in(N*N*N,2.0f);
	float *out = nullptr;

        /* std::cout << "output size = " << (-8 + (-6 * (int)pow((float)N, 2)) + (12 * N) + (int)pow((float)N, 3)) << std::endl; */

	lift::execute(in.data(), out, N);`

	copy(out,out+output_N*output_N*1, ostream_iterator<float>(cout, " "));
	std::cout << std::endl;
	
	return 0;
}
