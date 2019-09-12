#include <bits/stdc++.h>
#include "libreduce_3d_matrix.cpp"

using namespace std;

int main(int argc, char *argv[])
{

	int const N = 4;
	vector<float> in(N*N*N,1.0f);
	float *out = nullptr;

	lift::execute(in.data(), out, N);

	/* copy(out,out+N, ostream_iterator<float>(cout, ",")); */
	copy(out,out+1, ostream_iterator<float>(cout, " "));
	std::cout << std::endl;
	
	return 0;
}
