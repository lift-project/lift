
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
void degrees(float * v_initial_param_7140_2881, float * & v_user_func_7142_2882, int v_N_2763){
    // Allocate memory for output pointers
    v_user_func_7142_2882 = reinterpret_cast<float *>(malloc((v_N_2763 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_2880 = 0;(v_i_2880 <= (-1 + v_N_2763)); (++v_i_2880)){
        v_user_func_7142_2882[v_i_2880] = r2d_uf(v_initial_param_7140_2881[v_i_2880]); 
    }
}
}; 