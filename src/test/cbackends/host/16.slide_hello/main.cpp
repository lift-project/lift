#include <bits/stdc++.h>
#include "./libslide_hello.cpp"

using namespace std;

int main(int argc, char *argv[])
{

	int const N = 10;
	vector<float> in{1,2,3,4,5,6,7,8,9,10};
	float *out = nullptr;

	lift::execute(in.data(), out, N);

	copy(out,out + 3*(-2 + N), ostream_iterator<float>(cout, " "));
	std::cout << std::endl;
	
	return 0;
}
