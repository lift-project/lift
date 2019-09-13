#include <libgeneral_transpose.cpp>

int main() {


	//2-dimensional
	{
		/* const int M = 3, N = 4, size = M * N; */

		/* vector<float> x(size, 0); */
		/* iota(x.begin(), x.end(), 0); */

		/* copy(x.begin(), x.end(), ostream_iterator<float>(cout, ", ")); */
		/* std::cout << std::endl; */

		/* float *y = nullptr; */

		/* lift::execute(x.data(), y, N, M); */

		/* assert(y!=nullptr); */

		/* copy(y, y+size, ostream_iterator<float>(cout, ", ")); */
		/* cout<<endl; */
	}

	//3-dimensional
	{
		//const int M = 2, N = 2, K = 3, size = M * N * K;
		const int M = 3, N = 2, K = 4, size = M * N * K;

		vector<float> x(size, 0);
		iota(x.begin(), x.end(), 0);

		/* copy(x.begin(), x.end(), ostream_iterator<float>(cout, ", ")); */
		/* std::cout << std::endl; */

		float *y = nullptr;

		lift::execute(x.data(), y, N, M, K);

		assert(y!=nullptr);

		copy(y, y+size, ostream_iterator<float>(cout, ", "));
		cout<<endl;
	}

	return 0;
}
