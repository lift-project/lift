
#include <bits/stdc++.h>

using namespace std;

namespace lift {
    
#ifndef ARCTANH_UF_H
#define ARCTANH_UF_H
; 
float arctanh_uf(float x){
    { return atanh(x); }; 
}

#endif
 ; 
void arctanh(float * v_initial_param_207_95, float * & v_user_func_209_96, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_209_96 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_94 = 0;(v_i_94 <= (-1 + v_N_0)); (++v_i_94)){
        v_user_func_209_96[v_i_94] = arctanh_uf(v_initial_param_207_95[v_i_94]); 
    }
}
}; 