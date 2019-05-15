
#include <bits/stdc++.h>

using namespace std;

namespace lift {
    
#ifndef R2D_UF_H
#define R2D_UF_H
; 
float r2d_uf(float x){
    { return x*180/M_PI; }; 
}

#endif
 ; 
void rad2deg(float * v_initial_param_158_79, float * & v_user_func_160_80, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_160_80 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_78 = 0;(v_i_78 <= (-1 + v_N_0)); (++v_i_78)){
        v_user_func_160_80[v_i_78] = r2d_uf(v_initial_param_158_79[v_i_78]); 
    }
}
}; 