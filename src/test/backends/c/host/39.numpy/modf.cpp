#include <bits/stdc++.h>

#include <lift_numpy.hpp>

using namespace std;

int main(int argc, char *argv[])
{

	vector<float> x {0,-1.3,3,2.3};
	lift::Tuple2_float_float *r = nullptr;
	const int size = x.size();

	lift::modf(x.data(), r,size);
	/* lift::cos(x.data(),y,5); */

	//copy(r, r+size, ostream_iterator<float>(cout, " "));
	//std::cout << std::endl;
	std::cout << r[0]._0 << " " << r[0]._1 << " "
		<< r[1]._0 << " " << r[1]._1 << " "
		<< r[2]._0 << " " << r[2]._1 << " "
		<< r[3]._0 << " " << r[3]._1 << " " << std::endl;

	system("mkdir -p numpy_golden_data");
	system("./numpy/modf.py > numpy_golden_data/modf.txt");


	const int golden_data_size = size;
	vector<float> golden_data_flatten(golden_data_size*2, -999.99f);
	vector<lift::Tuple2_float_float> golden_data(golden_data_size, {-999.99,-999.99});

	ifstream file("numpy_golden_data/modf.txt");


	if (!file.good()) {
		fprintf(stderr, "Could not open the data file.\n");
		return( 1 );
	}

	istream_iterator<float> start(file), end;
	copy(start, end, golden_data_flatten.begin());

	for (int i = 0; i < golden_data_size ; ++i) {
		golden_data[i]._0 = golden_data_flatten[i];
		golden_data[i]._1 = golden_data_flatten[golden_data_size+i];
		/* std::cout << golden_data[i]._0 << " " << golden_data[i]._1 << std::endl; */
		
	}

	const float tol = 0.0001;

	for(int i = 0; i < golden_data_size; ++i)
	{
		/* std::cout << abs( golden_data[i] - y[i] ) << std::endl; */
		if( abs( golden_data[i]._0 - r[i]._0 ) > tol ){
			std::cout << "[modf]: Computed results does not match the golden data !!!" << std::endl;
			exit(1);
		}
		if( abs( golden_data[i]._1 - r[i]._1 ) > tol ){
			std::cout << "[modf]: Computed results does not match the golden data !!!" << std::endl;
			exit(1);
		}
	}



	return 0;
}
