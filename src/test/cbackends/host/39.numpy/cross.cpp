/* #include <bits/stdc++.h> */

#include <lift_numpy.hpp>

/* using namespace std; */

int main(int argc, char *argv[])
{

	vector<lift::Tuple3_float_float_float> x{  {3,5,4},{1,2,3} };
	vector<lift::Tuple3_float_float_float> y{  {2,7,5},{7,5,3} };

	lift::Tuple3_float_float_float *r = nullptr;
	const int size = x.size();

	lift::cross(x.data(), y.data(), r, size);
	/* lift::cos(x.data(),y,5); */

	/* copy(r, r+size, ostream_iterator<float>(cout, " ")); */
	/* std::cout << std::endl; */
	std::cout << r[0]._0 << " "<< r[0]._1 << " "<< r[0]._2 << " ";
	std::cout << r[1]._0 << " " << r[1]._1 << " " << r[1]._2 << " " << std::endl;

	system("mkdir -p numpy_golden_data");
	system("./numpy/cross.py > numpy_golden_data/cross.txt");


	const int golden_data_size = size;
	vector<float> golden_data_flatten(golden_data_size*3, -999.99);
	vector<lift::Tuple3_float_float_float> golden_data(golden_data_size, {-999,-999,-999});

	ifstream file("numpy_golden_data/cross.txt");

	if (!file.good()) {
		fprintf(stderr, "Could not open the data file.\n");
		return( 1 );
	}

	istream_iterator<float> start(file), end;
	copy(start, end, golden_data_flatten.begin());
	for (int i = 0; i < golden_data_size; ++i) {
		golden_data[i]._0 = golden_data_flatten[3*i];
		golden_data[i]._1 = golden_data_flatten[3*i+1];
		golden_data[i]._2 = golden_data_flatten[3*i+2];
		
	}

	const float tol = 0.0001;

	for(int i = 0; i < golden_data_size; ++i)
	{
		/* std::cout << abs( golden_data[i] - y[i] ) << std::endl; */
		if( abs( golden_data[i]._0 - r[i]._0 ) > tol ){
			std::cout << "[cross]: Computed results does not match the golden data !!!" << std::endl;
			exit(1);
		}
		if( abs( golden_data[i]._1 - r[i]._1 ) > tol ){
			std::cout << "[cross]: Computed results does not match the golden data !!!" << std::endl;
			exit(1);
		}
		if( abs( golden_data[i]._2 - r[i]._2 ) > tol ){
			std::cout << "[cross]: Computed results does not match the golden data !!!" << std::endl;
			exit(1);
		}
	}



	return 0;
}
