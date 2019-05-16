
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
void gradient(float * v_initial_param_329_159, float * & v_user_func_335_160, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_335_160 = reinterpret_cast<float *>(malloc(((-2 + v_N_0) * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_158 = 0;(v_i_158 <= (-3 + v_N_0)); (++v_i_158)){
        v_user_func_335_160[v_i_158] = grad2_uf(v_initial_param_329_159[(2 + v_i_158)], v_initial_param_329_159[v_i_158]); 
    }
}
}; 