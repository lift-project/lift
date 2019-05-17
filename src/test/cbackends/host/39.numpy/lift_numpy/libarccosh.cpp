
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef ARCCOS_UF_H
#define ARCCOS_UF_H
; 
float arccos_uf(float x){
    { return acos(x); }; 
}

#endif
 ; 
void arccosh(float * v_initial_param_128_117, float * & v_user_func_130_118, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_130_118 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_116 = 0;(v_i_116 <= (-1 + v_N_0)); (++v_i_116)){
        v_user_func_130_118[v_i_116] = arccos_uf(v_initial_param_128_117[v_i_116]); 
    }
}
}; 