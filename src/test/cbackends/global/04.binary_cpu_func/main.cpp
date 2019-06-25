#include <bits/stdc++.h>
#include "./libbinary_cpu_func.cpp"

using namespace std;

int main(int argc, char *argv[])
{

	int const M = 2;
	int const N = 6;
	vector<float> in(N,2.0f), in2(M, 1.0f);
	float *out = nullptr;

	lift::execute(in.data(), in2.data(), out, N, M);

	copy(out,out+N, ostream_iterator<float>(cout, " "));
	std::cout << std::endl;
	
	return 0;
}
