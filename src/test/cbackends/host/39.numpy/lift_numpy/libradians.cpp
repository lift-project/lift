
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef D2R_UF_H
#define D2R_UF_H
; 
float d2r_uf(float x){
    { return x*M_PI/180; }; 
}

#endif
 ; 
void radians(float * v_initial_param_1454_261, float * & v_user_func_1456_262, int v_N_190){
    // Allocate memory for output pointers
    v_user_func_1456_262 = reinterpret_cast<float *>(malloc((v_N_190 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_260 = 0;(v_i_260 <= (-1 + v_N_190)); (++v_i_260)){
        v_user_func_1456_262[v_i_260] = d2r_uf(v_initial_param_1454_261[v_i_260]); 
    }
}
}; 