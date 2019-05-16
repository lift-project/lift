
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
void trunc(float * v_initial_param_252_121, float * & v_user_func_254_122, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_254_122 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_120 = 0;(v_i_120 <= (-1 + v_N_0)); (++v_i_120)){
        v_user_func_254_122[v_i_120] = trunc_uf(v_initial_param_252_121[v_i_120]); 
    }
}
}; 