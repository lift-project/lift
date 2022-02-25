#include <libmc.cpp>

#include <bits/stdc++.h>
using namespace std;

int main() {

	const int size = 32;

	vector<float> x(size,1);

	iota(x.begin(), x.end(), 1);

	float *y = nullptr;

	lift::execute(x.data(), y);
	/* execute(x.data(), y); */

	assert(y!=nullptr);

	copy(y, y+1, ostream_iterator<float>(cout, " "));
	/* copy(y, y+16, ostream_iterator<float>(cout, " ")); */
	cout<<endl;

	return 0;
}
