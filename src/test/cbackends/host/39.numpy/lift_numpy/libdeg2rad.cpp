
#include <bits/stdc++.h>

using namespace std;

namespace lift {
    
#ifndef D2R_UF_H
#define D2R_UF_H
; 
float d2r_uf(float x){
    { return x*M_PI/180; }; 
}

#endif
 ; 
void deg2rad(float * v_initial_param_1454_264, float * & v_user_func_1456_265, int v_N_190){
    // Allocate memory for output pointers
    v_user_func_1456_265 = reinterpret_cast<float *>(malloc((v_N_190 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_263 = 0;(v_i_263 <= (-1 + v_N_190)); (++v_i_263)){
        v_user_func_1456_265[v_i_263] = d2r_uf(v_initial_param_1454_264[v_i_263]); 
    }
}
}; 