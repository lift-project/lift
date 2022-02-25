#include <libnumpy_diff.cpp>

int main() {

	const int size = 5;

	vector<float> x{1, 2, 4, 7, 0};

	float *y = nullptr;

	lift::execute(x.data(), y, size);

	assert(y!=nullptr);

	copy(y, y+size-1, ostream_iterator<float>(cout, " "));
	cout<<endl;

	return 0;
}
