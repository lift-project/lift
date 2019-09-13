#include <libpar_reduce_general.cpp>

int main() {

	{

		const int size1 = 7;

		vector<float> x(size1,1);

		float *z = nullptr;

		lift::execute(x.data(), z, size1);

		assert(z!=nullptr);

		copy(z, z+1, ostream_iterator<float>(cout, " "));
		cout<<endl;
	}


	{

		const int size1 = 13;

		vector<float> x(size1,1);

		float *z = nullptr;

		lift::execute(x.data(), z, size1);

		assert(z!=nullptr);

		copy(z, z+1, ostream_iterator<float>(cout, " "));
		cout<<endl;
	}

	return 0;
}
