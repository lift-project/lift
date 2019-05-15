
#include <bits/stdc++.h>

using namespace std;

namespace lift {
    
#ifndef RINT_UF_H
#define RINT_UF_H
; 
float rint_uf(float x){
    return round(x) ;; 
}

#endif
 ; 
void rint(float * v_initial_param_1510_294, float * & v_user_func_1512_295, int v_N_190){
    // Allocate memory for output pointers
    v_user_func_1512_295 = reinterpret_cast<float *>(malloc((v_N_190 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_293 = 0;(v_i_293 <= (-1 + v_N_190)); (++v_i_293)){
        v_user_func_1512_295[v_i_293] = rint_uf(v_initial_param_1510_294[v_i_293]); 
    }
}
}; 