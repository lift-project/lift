#include <bits/stdc++.h>
#include "./libviewmap.cpp"

using namespace std;

int main(int argc, char *argv[])
{

	int const N = 3;
	vector<float> in(N*N,1.0f);
	float *out = nullptr;

	lift::execute(in.data(), out, N);

	copy(out,out+N*N, ostream_iterator<float>(cout, " "));
	std::cout << std::endl;
	
	return 0;
}
