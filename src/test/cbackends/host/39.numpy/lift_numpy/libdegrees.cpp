
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
void degrees(float * v_initial_param_1447_258, float * & v_user_func_1449_259, int v_N_190){
    // Allocate memory for output pointers
    v_user_func_1449_259 = reinterpret_cast<float *>(malloc((v_N_190 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_257 = 0;(v_i_257 <= (-1 + v_N_190)); (++v_i_257)){
        v_user_func_1449_259[v_i_257] = r2d_uf(v_initial_param_1447_258[v_i_257]); 
    }
}
}; 