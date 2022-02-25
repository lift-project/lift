#include <bits/stdc++.h>

#include "./libfused_cpu.cpp"
#include "./libdmm1_cpu.cpp"
/* #include "./libdmm2_cpu.cpp" */

using namespace std;

int main(int argc, char *argv[])
{

	/* lift_init(); */

	int const K = 2;
	int const M = 3;
	int const N = 4;
	int const O = 5;
	vector<float> A(M*K,1.0f), B(N*K, 2.0f), C(M*N, 0.0f), C_temp(M*N, 0.0f), D(N*O, 3.0f), out(M*O, 0.0f), out2(M*O, 0.0f);
	/* float *out = nullptr; */
	iota(A.begin(), A.end(), 0);
	iota(B.begin(), B.end(), 1);
	iota(D.begin(), D.end(), 2);

	/* copy(A.begin(), A.end(), ostream_iterator<float>(cout, " ")); */
	/* std::cout << std::endl; */
	/* copy(B.begin(), B.end(), ostream_iterator<float>(cout, " ")); */
	/* std::cout << std::endl; */
	/* copy(D.begin(), D.end(), ostream_iterator<float>(cout, " ")); */
	/* std::cout << std::endl; */

	/* lift::execute(A.data(), B.data(), D.data(), out); */
	execute(A.data(), B.data(), D.data(), out.data(), C_temp.data(), K, M, N, O);

	copy(out.begin(), out.end(), ostream_iterator<float>(cout, " "));
	std::cout << std::endl;

	execute1(A.data(), B.data(), C.data(), K, M, N);
	copy(C.begin(), C.end(), ostream_iterator<float>(cout, " "));
	std::cout << std::endl;
	execute1(C.data(), D.data(), out2.data(), N, M, O);
	/* execute1(A.data(), B.data(), C.data()); */
	/* execute2(C.data(), D.data(), out2.data()); */

	copy(out2.begin(), out2.end(), ostream_iterator<float>(cout, " "));
	std::cout << std::endl;

	for(int i = 0; i< out.size(); ++i)
		assert(out[i] == out2[i]);
	
	return 0;
}
