#include <libcross_prod.cpp>

int main() {

	const int size = 2;

	vector<lift::Tuple3_float_float_float> x{  {3,5,4},{1,2,3} };
	vector<lift::Tuple3_float_float_float> y{  {2,7,5},{7,5,3} };

	lift::Tuple3_float_float_float *z = nullptr;

	lift::execute(x.data(), y.data(), z, size);

	assert(z!=nullptr);

	std::cout << z[0]._0 << " "<< z[0]._1 << " "<< z[0]._2 << " ";
	std::cout << z[1]._0 << " " << z[1]._1 << " " << z[1]._2 << " " << std::endl;

	return 0;
}
