#include <bits/stdc++.h>

#include "./libfused.cpp"
#include "./libdmm1.cpp"

using namespace std;

int main(int argc, char *argv[])
{

	lift_init();
	init_kernel();

	int const K = 2;
	int const M = 16;
	int const N = 32;
	int const O = 8;
	vector<float> A(M*K,1.0f), B(N*K, 2.0f), C_temp(M*N, 0.0f), D(N*O, 3.0f);
	/* vector<float> out(M*O, 0.0f), out2(M*O, 0.0f), C(M*N, 0.0f); */
	float *out = nullptr, *out2 = nullptr, *C = nullptr;
	iota(A.begin(), A.end(), 0);
	iota(B.begin(), B.end(), 1);
	iota(D.begin(), D.end(), 2);


	lift::execute(A.data(), B.data(), D.data(), out, M, K, N, O);
	/* copy(out, out + M*O, ostream_iterator<float>(cout, " ")); */
	/* std::cout << std::endl; */


	lift::execute1(A.data(), B.data(), C, M, K, N);
	/* lift::execute1(A.data(), B.data(), C, M, K, N); */
	/* copy(C, C + M*N, ostream_iterator<float>(cout, " ")); */
	/* std::cout << std::endl; */
	lift::execute1(C, D.data(), out2, M, N, O);
	/* copy(out2, out2 + M*O, ostream_iterator<float>(cout, " ")); */
	/* std::cout << std::endl; */
	/*
	execute1(C.data(), D.data(), out2.data(), N, M, O);

	copy(out2.begin(), out2.end(), ostream_iterator<float>(cout, " "));
	std::cout << std::endl;
	*/

	for(int i = 0; i< M*O; ++i)
		assert(out[i] == out2[i]);
	
	
	return 0;
}
