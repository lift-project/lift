#include <bits/stdc++.h>
#include "./libslide2d.cpp"

using namespace std;

int main(int argc, char *argv[])
{

	int const N = 10;
	int const output_N = (4 + (-4 * N) + (int)pow((float)N, 2));
	vector<float> in(N*N,1.0f);
	float *out = nullptr;

	lift::execute(in.data(), out, N);

	copy(out,out+output_N, ostream_iterator<float>(cout, " "));
	std::cout << std::endl;
	
	return 0;
}
