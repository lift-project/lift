#include <bits/stdc++.h>
#include "./libconcrete_non2d_concrete.cpp"

using namespace std;

int main(int argc, char *argv[])
{

	int const N = 4;
	vector<float> in(N*N,1.0f);
	float *out = nullptr;

	execute(in.data(), out, N);

	copy(out,out+(N-2)*(N-2), ostream_iterator<float>(cout, " "));
	std::cout << std::endl;
	
	return 0;
}
