
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
void trunc(float * v_initial_param_265_135, float * & v_user_func_267_136, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_267_136 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_134 = 0;(v_i_134 <= (-1 + v_N_0)); (++v_i_134)){
        v_user_func_267_136[v_i_134] = trunc_uf(v_initial_param_265_135[v_i_134]); 
    }
}
}; 