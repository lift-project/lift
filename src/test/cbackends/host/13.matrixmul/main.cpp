#include <libmatrixmul.cpp>

int main() {

	const int K=8, M=2, N=2;
	const int size = K*8;

	vector<float> x(size,1), y(size,1);

	float *z = nullptr;

	execute(x.data(), y.data(), z, K, M, N);

	assert(z!=nullptr);

	copy(z, z+M*N, ostream_iterator<float>(cout, " "));
	cout<<endl;

	return 0;
}
