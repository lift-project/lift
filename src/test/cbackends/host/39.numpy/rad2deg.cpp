#include <bits/stdc++.h>

#include <lift_numpy.hpp>

using namespace std;

int main(int argc, char *argv[])
{

	vector<float> x {0, 0.52359878, 1.04719755, 1.57079633, 2.0943951, 2.61799388, 3.14159265, 3.66519143, 4.1887902 , 4.71238898, 5.23598776, 5.75958653};
	float *y = nullptr;
	const int size = x.size();

	lift::rad2deg(x.data(),y,size);
	/* lift::cos(x.data(),y,5); */

	copy(y, y+size, ostream_iterator<float>(cout, " "));
	std::cout << std::endl;

	system("mkdir -p numpy_golden_data");
	system("./numpy/rad2deg.py > numpy_golden_data/rad2deg.txt");


	const int golden_data_size = size;
	vector<float> golden_data(golden_data_size, -999.99);

	ifstream file("numpy_golden_data/rad2deg.txt");

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
		if( abs( golden_data[i] - y[i] ) > tol ){
			std::cout << "[rad2deg]: Computed results does not match the golden data !!!" << std::endl;
			exit(1);
		}
	}



	return 0;
}
