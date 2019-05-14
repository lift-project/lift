#include <libscan.cpp>

int main() {

	const int size = 16;

	vector<float> x(size,1);

	float *y = nullptr;

	lift::execute(x.data(), y, size);

	assert(y!=nullptr);

	copy(y, y+size, ostream_iterator<float>(cout, " "));
	cout<<endl;

	return 0;
}
