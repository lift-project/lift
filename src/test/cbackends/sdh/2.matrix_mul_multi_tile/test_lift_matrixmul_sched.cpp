#include <bits/stdc++.h>
#include "test_lift_matrixmul_sched_lib.hpp" 

using namespace std;

/* ------------------------------------------------------------ */
/* Control pannel */

#define SIZE 16
#define K 4
#define M 4
#define N 4
/* ------------------------------------------------------------ */

int main(int argc, char *argv[]) {

    int const size = SIZE;
    

    float* input1 = reinterpret_cast<float*>(trans_alloc(size*sizeof(float)));
    float* input2 = reinterpret_cast<float*>(trans_alloc(size*sizeof(float)));
    float* output = nullptr;

    fill(input1, input1+size, 1);
    fill(input2, input2+size, 2);


    /*LCP_PRINTF("input[] = {");
    copy(input, input+size, ostream_iterator<int>(cout, " ") );
    printf("}\n");
    cout<<"[ SIZE = ] "<<size<<endl;
    */

    //calling the code generated
    execute(input1, input2, output, K, M, N);

    // Print the result.
    LCP_PRINTF("output[] = {");
    for(int i = 0; i < size; ++i) {
        printf("%f ", output[i]);
    }
    printf("}\n");
    


    return 0;
}
