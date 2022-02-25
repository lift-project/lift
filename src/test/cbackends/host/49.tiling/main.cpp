#include <bits/stdc++.h>
#include "./libtiling.cpp"

using namespace std;

int main(int argc, char *argv[])
{

	int const N = 6, M = 4;
	vector<float> in(N*M,1.0f);
	float *out = nullptr;

	lift::execute(in.data(), out, N, M);

	copy(out,out+N*M, ostream_iterator<float>(cout, " "));
	std::cout << std::endl;
	
	return 0;
}
