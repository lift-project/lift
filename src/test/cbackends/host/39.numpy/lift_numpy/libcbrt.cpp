
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef CBRT_UF_H
#define CBRT_UF_H
; 
float cbrt_uf(float x){
    { return cbrt(x); }; 
}

#endif
 ; 
void cbrt(float * v_initial_param_880_385, float * & v_user_func_882_386, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_882_386 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_384 = 0;(v_i_384 <= (-1 + v_N_0)); (++v_i_384)){
        v_user_func_882_386[v_i_384] = cbrt_uf(v_initial_param_880_385[v_i_384]); 
    }
}
}; 