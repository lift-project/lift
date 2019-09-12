#include <bits/stdc++.h>

#include <lift_numpy.hpp>

using namespace std;

int main(int argc, char *argv[])
{

	vector<float> x {1,2,3,4}, y{2,3,4,5};
	float *r = nullptr;
	const int size = x.size();

	lift::ldexp(x.data(), y.data(),r,size);
	/* lift::cos(x.data(),y,5); */

	copy(r, r+size, ostream_iterator<float>(cout, " "));
	std::cout << std::endl;

	system("mkdir -p numpy_golden_data");
	system("./numpy/ldexp.py > numpy_golden_data/ldexp.txt");


	const int golden_data_size = size;
	vector<float> golden_data(golden_data_size, -999.99);

	ifstream file("numpy_golden_data/ldexp.txt");

	if (!file.good()) {
		fprintf(stderr, "Could not open the data file.\n");
		return( 1 );
	}

	istream_iterator<float> start(file), end;
	copy(start, end, golden_data.begin());

	const float tol = 0.0001;

	for(int i = 0; i < golden_data_size; ++i)
	{
		/* std::cout << abs( golden_data[i] - y[i] ) << std::endl; */
		if( abs( golden_data[i] - r[i] ) > tol ){
			std::cout << "[ldexp]: Computed results does not match the golden data !!!" << std::endl;
			std::cout << "------------------------------------------------------------" << std::endl;
			std::cout << "golden value:   " << std::endl;
			copy(golden_data.begin(), golden_data.end(), ostream_iterator<float>(cout, " "));
			std::cout << std::endl;
			std::cout << "computed value: " << std::endl;
			copy(r, r+size, ostream_iterator<float>(cout, " "));
			std::cout << std::endl;
			std::cout << "------------------------------------------------------------" << std::endl;
			exit(1);
		}
	}



	return 0;
}
