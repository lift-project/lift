#include <bits/stdc++.h>

#include <lift_numpy.hpp>

using namespace std;

int main(int argc, char *argv[])
{

	//notice this is a 2-diemensional array, 2 by 2
	vector<float> x {0,1,0,5};
	float *r = nullptr;
	const int size = x.size();

	lift::sum_axis_1(x.data(), r, 2, 2);
	/* lift::cos(x.data(),y,5); */

	copy(r, r+2, ostream_iterator<float>(cout, " "));
	std::cout << std::endl;

	system("mkdir -p numpy_golden_data");
	system("./numpy/sum_axis_1.py > numpy_golden_data/sum_axis_1.txt");


	const int golden_data_size = 2;
	vector<float> golden_data(golden_data_size, -999.99);

	ifstream file("numpy_golden_data/sum_axis_1.txt");

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
			std::cout << "[sum_axix_1]: Computed results does not match the golden data !!!" << std::endl;
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
