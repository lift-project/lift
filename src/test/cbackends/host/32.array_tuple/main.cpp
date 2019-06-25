#include "libarray_tuple.cpp"

int main() {

	const int size = 2;

	vector<Tuple2_float_float> x{  {1,2},{3,4} };

	Tuple2_float_float *z = nullptr;

	execute(x.data(), z, size);

	assert(z!=nullptr);

	std::cout << z[0]._0 << ", "<< z[0]._1 << ", ";
	std::cout << z[1]._0 << ", " << z[1]._1 << std::endl;

	return 0;
}
