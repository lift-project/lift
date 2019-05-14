#include <bits/stdc++.h>
#include "./libslide3d.cpp"

using namespace std;

int main(int argc, char *argv[])
{

	int const N = 3;
	int const output_N = 1;
	vector<float> in(N*N*N,1.0f);
	float *out = nullptr;

	lift::execute(in.data(), out, N);

	copy(out,out+output_N, ostream_iterator<float>(cout, " "));
	std::cout << std::endl;
	
	return 0;
}
