
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
void rad2deg(float * v_initial_param_7140_2890, float * & v_user_func_7142_2891, int v_N_2763){
    // Allocate memory for output pointers
    v_user_func_7142_2891 = reinterpret_cast<float *>(malloc((v_N_2763 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_2889 = 0;(v_i_2889 <= (-1 + v_N_2763)); (++v_i_2889)){
        v_user_func_7142_2891[v_i_2889] = r2d_uf(v_initial_param_7140_2890[v_i_2889]); 
    }
}
}; 