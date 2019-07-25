
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
void fix(float * v_initial_param_3266_509, float * & v_user_func_3268_510, int v_N_352){
    // Allocate memory for output pointers
    v_user_func_3268_510 = reinterpret_cast<float *>(malloc((v_N_352 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_508 = 0;(v_i_508 <= (-1 + v_N_352)); (++v_i_508)){
        v_user_func_3268_510[v_i_508] = fix_uf(v_initial_param_3266_509[v_i_508]); 
    }
}
}; 