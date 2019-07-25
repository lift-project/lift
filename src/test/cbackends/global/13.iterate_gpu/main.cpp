#include <bits/stdc++.h>
#include "./libiterate.cpp"

using namespace std;

int main(int argc, char *argv[])
{
	lift_init();

	int const N = 2;
	vector<float> in(N,1.0f);
	float *out = nullptr;

	lift::execute(in.data(), out, N);

	copy(out,out+N, ostream_iterator<float>(cout, " "));
	std::cout << std::endl;
	
	return 0;
}
