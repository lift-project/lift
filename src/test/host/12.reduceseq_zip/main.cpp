#include <libreduceseq_zip.cpp>

int main() {

	const int size = 16;

	vector<double> x(size,1), y(size,2);

	std::tuple<double,double> *z = nullptr;

	execute(x.data(), y.data(), z, size);

	assert(z!=nullptr);

	cout<<std::get<0>(z[0])<<" "<<std::get<1>(z[0])<<" ";
	cout<<endl;

	return 0;
}
