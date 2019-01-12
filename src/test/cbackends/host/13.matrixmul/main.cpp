#include <libmatrixmul.cpp>

int main() {

	const int K=8, M=2, N=2;

	vector<float> x(K*M,1), y(K*N,1);

	float *z = nullptr;

	execute(x.data(), y.data(), z, K, M, N);

	assert(z!=nullptr);

	copy(z, z+M*N, ostream_iterator<float>(cout, " "));
	cout<<endl;

	return 0;
}
