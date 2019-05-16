
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
void gradient(float * v_initial_param_336_166, float * & v_user_func_342_167, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_342_167 = reinterpret_cast<float *>(malloc(((-2 + v_N_0) * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_165 = 0;(v_i_165 <= (-3 + v_N_0)); (++v_i_165)){
        v_user_func_342_167[v_i_165] = grad2_uf(v_initial_param_336_166[(2 + v_i_165)], v_initial_param_336_166[v_i_165]); 
    }
}
}; 