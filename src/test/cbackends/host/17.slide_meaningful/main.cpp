#include <bits/stdc++.h>
#include "./libslide_meaningful.cpp"

using namespace std;

int main(int argc, char *argv[])
{

	int const N = 10;
	vector<float> in(N,1.0f);
	float *out = nullptr;

	lift::execute(in.data(), out, N);

	copy(out,out+N-2, ostream_iterator<float>(cout, " "));
	std::cout << std::endl;
	
	return 0;
}
