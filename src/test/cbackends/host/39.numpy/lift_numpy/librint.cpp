
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef RINT_UF_H
#define RINT_UF_H
; 
float rint_uf(float x){
    return round(x) ;; 
}

#endif
 ; 
void rint(float * v_initial_param_242_129, float * & v_user_func_244_130, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_244_130 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_128 = 0;(v_i_128 <= (-1 + v_N_0)); (++v_i_128)){
        v_user_func_244_130[v_i_128] = rint_uf(v_initial_param_242_129[v_i_128]); 
    }
}
}; 