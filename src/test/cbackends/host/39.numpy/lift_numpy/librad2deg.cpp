
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef R2D_UF_H
#define R2D_UF_H
; 
float r2d_uf(float x){
    { return x*180/M_PI; }; 
}

#endif
 ; 
void rad2deg(float * v_initial_param_189_114, float * & v_user_func_191_115, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_191_115 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_113 = 0;(v_i_113 <= (-1 + v_N_0)); (++v_i_113)){
        v_user_func_191_115[v_i_113] = r2d_uf(v_initial_param_189_114[v_i_113]); 
    }
}
}; 