#include <bits/stdc++.h>

#include <lift_numpy.hpp>

using namespace std;

int main(int argc, char *argv[])
{

	vector<float> x {0,90,180,270,360};
	float *y = nullptr;

	lift::sin(x.data(),y,5);
	lift::cos(x.data(),y,5);

	copy(y, y+5, ostream_iterator<float>(cout, " "));
	std::cout << std::endl;
	


	return 0;
}
