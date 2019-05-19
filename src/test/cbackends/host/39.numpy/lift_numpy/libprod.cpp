
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef PROD2_UF_H
#define PROD2_UF_H
; 
float prod2_uf(float l, float r){
    { return (l * r); }; 
}

#endif
 ; 
void prod(float * v_initial_param_298_165, float * & v_user_func_301_166, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_301_166 = reinterpret_cast<float *>(malloc((1 * sizeof(float)))); 
    // For each element reduced sequentially
    v_user_func_301_166[0] = 1.0f; 
    for (int v_i_164 = 0;(v_i_164 <= (-1 + v_N_0)); (++v_i_164)){
        v_user_func_301_166[0] = prod2_uf(v_user_func_301_166[0], v_initial_param_298_165[v_i_164]); 
    }
}
}; 