
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef GRAD2_UF_H
#define GRAD2_UF_H
; 
float grad2_uf(float l, float r){
    { return (l - r)/2.0f; }; 
}

#endif
 ; 
void gradient(float * v_initial_param_338_169, float * & v_user_func_344_170, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_344_170 = reinterpret_cast<float *>(malloc(((-2 + v_N_0) * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_168 = 0;(v_i_168 <= (-3 + v_N_0)); (++v_i_168)){
        v_user_func_344_170[v_i_168] = grad2_uf(v_initial_param_338_169[(2 + v_i_168)], v_initial_param_338_169[v_i_168]); 
    }
}
}; 