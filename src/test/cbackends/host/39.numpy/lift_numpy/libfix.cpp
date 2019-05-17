
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
void fix(float * v_initial_param_251_134, float * & v_user_func_253_135, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_253_135 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_133 = 0;(v_i_133 <= (-1 + v_N_0)); (++v_i_133)){
        v_user_func_253_135[v_i_133] = fix_uf(v_initial_param_251_134[v_i_133]); 
    }
}
}; 