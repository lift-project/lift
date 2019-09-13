#include <libconcat_slice.cpp>

int main() {

	const int size1 = 8;

	vector<float> x(size1,1);

	float *z = nullptr;

	lift::execute(x.data(), z, size1);

	assert(z!=nullptr);

	copy(z, z+size1, ostream_iterator<float>(cout, " "));
	cout<<endl;

	return 0;
}
