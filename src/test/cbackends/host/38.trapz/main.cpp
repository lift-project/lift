#include <libtrapz.cpp>

int main() {

	const int size = 3;

	vector<float> x{1,2,3}, y{4,5,6};

	float *z = nullptr;

	lift::execute(x.data(), y.data(), z, size);

	assert(z!=nullptr);

	copy(z, z+size-1, ostream_iterator<float>(cout, " "));
	cout<<endl;

	return 0;
}
