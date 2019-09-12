#include <libtranspose_tranposew.cpp>

int main() {

	const int size = 16;

	vector<float> x(size,0);
	iota(x.begin(), x.end(), 0);

	float *y = nullptr;

	lift::execute(x.data(), y, size);

	assert(y!=nullptr);

	copy(y, y+size, ostream_iterator<float>(cout, " "));
	cout<<endl;

	return 0;
}
