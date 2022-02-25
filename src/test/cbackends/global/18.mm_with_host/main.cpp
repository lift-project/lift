#include <bits/stdc++.h>
#include "./libgpufunc.cpp"

using namespace std;

int main(int argc, char *argv[])
{

	lift_init();

	int const N = 15;
	int const M = 10;
	int const K = 5;
	vector<float> A(N*K,1.0f), B(M*K, 2.0f);
	float *out = nullptr;

	lift::execute(A.data(), B.data(), out);

	copy(out,out+15*10, ostream_iterator<float>(cout, " "));
	std::cout << std::endl;
	
	return 0;
}
