
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
void fix(float * v_initial_param_254_137, float * & v_user_func_256_138, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_256_138 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_136 = 0;(v_i_136 <= (-1 + v_N_0)); (++v_i_136)){
        v_user_func_256_138[v_i_136] = fix_uf(v_initial_param_254_137[v_i_136]); 
    }
}
}; 