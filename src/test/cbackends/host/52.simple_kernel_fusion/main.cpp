#include <bits/stdc++.h>

using namespace std;

#include <libfusion.cpp>

int main() {

	const int size = 9;

	vector<float> x(size,1);

	float *y = nullptr;

	lift::execute(x.data(), y);

	assert(y!=nullptr);

	copy(y, y+1, ostream_iterator<float>(cout, " "));
	cout<<endl;

	return 0;
}
