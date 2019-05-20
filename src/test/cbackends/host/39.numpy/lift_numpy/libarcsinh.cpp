
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef ARCSINH_UF_H
#define ARCSINH_UF_H
; 
float arcsinh_uf(float x){
    { return asinh(x); }; 
}

#endif
 ; 
void arcsinh(float * v_initial_param_235_139, float * & v_user_func_237_140, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_237_140 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_138 = 0;(v_i_138 <= (-1 + v_N_0)); (++v_i_138)){
        v_user_func_237_140[v_i_138] = arcsinh_uf(v_initial_param_235_139[v_i_138]); 
    }
}
}; 