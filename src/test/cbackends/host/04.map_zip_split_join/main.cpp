#include <libmap_zip_split_join.cpp>

int main() {

	const int size = 16;

	vector<float> x(size,1), y(size,2);

	float *z = nullptr;

	lift::execute(x.data(), y.data(), z, size);

	assert(z!=nullptr);

	copy(z, z+size, ostream_iterator<float>(cout, " "));
	cout<<endl;

	return 0;
}
