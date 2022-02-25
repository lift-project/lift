#include <bits/stdc++.h>
#include "./libtiling.cpp"

using namespace std;

/* #define BASELINE */

int main(int argc, char *argv[])
{

	int const N = 9, M = 16;
	vector<float> A(N), B(M), temp(N*M);
#ifdef BASELINE
	float *out = nullptr;
#else
	vector<float> out(N*M);
#endif

	iota(A.begin(), A.end(), 0);
	iota(B.begin(), B.end(), 0);
	/* copy(A.begin(),A.end() , ostream_iterator<float>(cout, " ")); */
	/* std::cout << std::endl; */
	/* copy(B.begin(),B.end() , ostream_iterator<float>(cout, " ")); */
	/* std::cout << std::endl; */


#ifdef BASELINE
	lift::execute(A.data(), B.data(), out  );
	copy(out,out+N*M, ostream_iterator<float>(cout, " "));
#else
	execute(A.data(), B.data(), out.data(), temp.data() );
	copy(out.begin(),out.end(), ostream_iterator<float>(cout, " "));
#endif

	std::cout << std::endl;
	
	return 0;
}
