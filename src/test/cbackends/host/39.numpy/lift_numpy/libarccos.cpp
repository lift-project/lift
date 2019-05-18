
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
void arccos(float * v_initial_param_138_87, float * & v_user_func_140_88, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_140_88 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_86 = 0;(v_i_86 <= (-1 + v_N_0)); (++v_i_86)){
        v_user_func_140_88[v_i_86] = arccos_uf(v_initial_param_138_87[v_i_86]); 
    }
}
}; 