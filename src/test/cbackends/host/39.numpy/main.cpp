#include <bits/stdc++.h>

#include <lift_numpy.hpp>

using namespace std;

int main(int argc, char *argv[])
{

	vector<float> x {0,90,180,270,360};
	float *y = nullptr;
	const int size = 5;

	lift::sin(x.data(),y,size);
	/* lift::cos(x.data(),y,5); */

	copy(y, y+size, ostream_iterator<float>(cout, " "));
	std::cout << std::endl;

	system("./main.py > python_golden.txt");


	const int golden_data_size = size;
	vector<float> golden_data(golden_data_size, -999.99);

	ifstream file("python_golden.txt");

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
		if( abs( golden_data[i] - y[i] ) > tol )
			std::cout << "Computed results does not match the golden data !!!" << std::endl;
	}



	return 0;
}