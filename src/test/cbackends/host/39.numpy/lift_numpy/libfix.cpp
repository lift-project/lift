
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
void fix(float * v_initial_param_241_122, float * & v_user_func_243_123, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_243_123 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_121 = 0;(v_i_121 <= (-1 + v_N_0)); (++v_i_121)){
        v_user_func_243_123[v_i_121] = fix_uf(v_initial_param_241_122[v_i_121]); 
    }
}
}; 