#include <libpad.cpp>

int main() {

	const int size = 16;

	vector<float> x(size,0);

	float *y = nullptr;

	execute(x.data(), y, size);

	assert(y!=nullptr);

	//Notice that the output array are sligtly larger
	//than the input array, because of the padding
	//only influence the output array
	copy(y, y+size+2, ostream_iterator<float>(cout, " "));
	cout<<endl;

	return 0;
}
