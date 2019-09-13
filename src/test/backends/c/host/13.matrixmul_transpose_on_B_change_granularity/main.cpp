#include <libmatrixmul.cpp>

int main() {

	const int K=3, M=4, N=8;

	vector<float> x(K*M), y(K*N);
	iota(x.begin(), x.end(), 0);
	iota(y.begin(), y.end(), 1);

	/* copy(x.begin(), x.end(), ostream_iterator<float>(cout, " ")); */
	/* std::cout << std::endl; */
	/* copy(y.begin(), y.end(), ostream_iterator<float>(cout, " ")); */
	/* std::cout << std::endl; */ 

	float *z = nullptr;

	lift::execute(x.data(), y.data(), z,  M, K, N);

	assert(z!=nullptr);

	copy(z, z+M*N, ostream_iterator<float>(cout, " "));
	cout<<endl;

	return 0;
}
