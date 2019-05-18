
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
void arccosh(float * v_initial_param_138_129, float * & v_user_func_140_130, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_140_130 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_128 = 0;(v_i_128 <= (-1 + v_N_0)); (++v_i_128)){
        v_user_func_140_130[v_i_128] = arccos_uf(v_initial_param_138_129[v_i_128]); 
    }
}
}; 