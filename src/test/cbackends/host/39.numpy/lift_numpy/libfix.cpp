
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef FIX_UF_H
#define FIX_UF_H
; 
float fix_uf(float x){
    return trunc(x) ;; 
}

#endif
 ; 
void fix(float * v_initial_param_247_130, float * & v_user_func_249_131, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_249_131 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_129 = 0;(v_i_129 <= (-1 + v_N_0)); (++v_i_129)){
        v_user_func_249_131[v_i_129] = fix_uf(v_initial_param_247_130[v_i_129]); 
    }
}
}; 