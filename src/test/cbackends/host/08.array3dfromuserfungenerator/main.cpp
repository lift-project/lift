#include <libarray3dfromuserfungenerator.cpp>

int main() {

	const int n1 = 1, n2 = 2, n3 = 3;
	const int size = n1 * n2 * n3;

	vector<float> x(size,0);

	float *y = nullptr;

	lift::execute(x.data(), y, n1, n2, n3);

	assert(y!=nullptr);

	copy(y, y+size, ostream_iterator<float>(cout, " "));
	cout<<endl;

	return 0;
}
