#include <bits/stdc++.h>
#include "test_lift_matrixmul_sched_lib.hpp" 

using namespace std;

/* ------------------------------------------------------------ */
/* Control pannel */

#define K 3
#define M 2
#define N 4
/* ------------------------------------------------------------ */

int main(int argc, char *argv[]) {

    int const sizeA = M*K;
    int const sizeB = N*K;
    int const sizeC = M*N;
    

    float* inputA = reinterpret_cast<float*>(trans_alloc(sizeA*sizeof(float)));
    float* inputB = reinterpret_cast<float*>(trans_alloc(sizeB*sizeof(float)));
    float* output = nullptr;

    fill(inputA, inputA+sizeA, 1);
    fill(inputB, inputB+sizeB, 2);

    cout << "[ SIZE_A = ] " << sizeA << endl;
    cout << "inputA[] = {" ;
    copy(inputA, inputA+sizeA, ostream_iterator<float>(cout, " ") );
    cout << "}" << endl;
    cout << "[ SIZE_B = ] " << sizeB << endl;
    cout << "inputB[] = {" ;
    copy(inputB, inputB+sizeB, ostream_iterator<float>(cout, " ") );
    cout << "}" << endl;


    /*LCP_PRINTF("input[] = {");
    copy(input, input+size, ostream_iterator<int>(cout, " ") );
    printf("}\n");
    cout<<"[ SIZE = ] "<<size<<endl;
    */

    //calling the code generated
    execute(inputA, inputB, output, K, M, N);

    // Print the result.
    LCP_PRINTF("output[] = {");
    for(int i = 0; i < sizeC; ++i) {
        printf("%f ", output[i]);
    }
    printf("}\n");
    


    return 0;
}
