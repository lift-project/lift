
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
void fix(float * v_initial_param_249_132, float * & v_user_func_251_133, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_251_133 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_131 = 0;(v_i_131 <= (-1 + v_N_0)); (++v_i_131)){
        v_user_func_251_133[v_i_131] = fix_uf(v_initial_param_249_132[v_i_131]); 
    }
}
}; 