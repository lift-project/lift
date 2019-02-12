#include <bits/stdc++.h>
#include "./libconcrete_nonTranspose_concrete.cpp"

using namespace std;

int main(int argc, char *argv[])
{

	int const N = 2;
	int const M = 3;
	vector<float> in(N*N,1.0f);
	float *out = nullptr;

	execute(in.data(), out, M, N);

	copy(out,out+N*M, ostream_iterator<float>(cout, " "));
	std::cout << std::endl;
	
	return 0;
}
