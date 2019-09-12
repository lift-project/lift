#include <bits/stdc++.h>
#include "./opencl_scalar_dot_host.cpp"

using namespace std;

int main(int argc, char *argv[])
{

	int const N = 16;
	vector<float> input(N);
	std::iota(std::begin(input), std::end(input), 0); // 0..N
	float *out = nullptr;

        /* std::cout << "output size = " << (-8 + (-6 * (int)pow((float)N, 2)) + (12 * N) + (int)pow((float)N, 3)) << std::endl; */

    lift_init();
	lift::execute(input.data(), input.data(), out);

	copy(out, out + 1, ostream_iterator<float>(cout, " "));
	std::cout << std::endl;
	
	return 0;
}
