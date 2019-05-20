
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
void arcsinh(float * v_initial_param_235_141, float * & v_user_func_237_142, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_237_142 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_140 = 0;(v_i_140 <= (-1 + v_N_0)); (++v_i_140)){
        v_user_func_237_142[v_i_140] = arcsinh_uf(v_initial_param_235_141[v_i_140]); 
    }
}
}; 