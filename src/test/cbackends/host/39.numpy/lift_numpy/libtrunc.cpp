
#include <bits/stdc++.h>

using namespace std;

namespace lift {
    
#ifndef TRUNC_UF_H
#define TRUNC_UF_H
; 
float trunc_uf(float x){
    return trunc(x);; 
}

#endif
 ; 
void trunc(float * v_initial_param_1538_306, float * & v_user_func_1540_307, int v_N_190){
    // Allocate memory for output pointers
    v_user_func_1540_307 = reinterpret_cast<float *>(malloc((v_N_190 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_305 = 0;(v_i_305 <= (-1 + v_N_190)); (++v_i_305)){
        v_user_func_1540_307[v_i_305] = trunc_uf(v_initial_param_1538_306[v_i_305]); 
    }
}
}; 