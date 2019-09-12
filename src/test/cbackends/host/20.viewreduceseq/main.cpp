#include <bits/stdc++.h>
#include "./libviewreduce.cpp"

using namespace std;

int main(int argc, char *argv[])
{

	int const N = 3;
	vector<float> in(N*N,1.0f);
	float *out = nullptr;

	lift::execute(in.data(), out, N);

	copy(out,out+1, ostream_iterator<float>(cout, " "));
	std::cout << std::endl;
	
	return 0;
}
