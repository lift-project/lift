#include <bits/stdc++.h>
#include "./libcpufunc.cpp"

using namespace std;

int main(int argc, char *argv[])
{

	int const M = 3;
	int const N = 2;
	vector<float> in(N*M,1.0f);
	float *out = nullptr;

	lift::execute(in.data(), out, M, N);

	copy(out,out+N*M, ostream_iterator<float>(cout, " "));
	std::cout << std::endl;
	
	return 0;
}
