
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef TRUNC_UF_H
#define TRUNC_UF_H
; 
float trunc_uf(float x){
    return trunc(x);; 
}

#endif
 ; 
void trunc(float * v_initial_param_256_125, float * & v_user_func_258_126, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_258_126 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_124 = 0;(v_i_124 <= (-1 + v_N_0)); (++v_i_124)){
        v_user_func_258_126[v_i_124] = trunc_uf(v_initial_param_256_125[v_i_124]); 
    }
}
}; 