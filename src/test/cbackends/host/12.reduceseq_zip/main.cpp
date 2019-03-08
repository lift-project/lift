#include <libreduceseq_zip.cpp>

int main() {

	const int size = 16;

	vector<double> x(size,1), y(size,2);

	Tuple2_double_double *z = nullptr;

	execute(x.data(), y.data(), z, size);

	assert(z!=nullptr);

	cout<<z[0]._0<<" "<<z[0]._1<<" ";
	cout<<endl;

	return 0;
}
