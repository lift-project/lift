
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
void trunc(float * v_initial_param_291_168, float * & v_user_func_293_169, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_293_169 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_167 = 0;(v_i_167 <= (-1 + v_N_0)); (++v_i_167)){
        v_user_func_293_169[v_i_167] = trunc_uf(v_initial_param_291_168[v_i_167]); 
    }
}
}; 