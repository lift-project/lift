#include <libconcat.cpp>

int main() {

	const int size1 = 3, size2 = 5;

	vector<float> x(size1,1), y(size2,2);

	float *z = nullptr;

	lift::execute(x.data(), y.data(), z, size1, size2);

	assert(z!=nullptr);

	copy(z, z+size1+size2, ostream_iterator<float>(cout, " "));
	cout<<endl;

	return 0;
}
