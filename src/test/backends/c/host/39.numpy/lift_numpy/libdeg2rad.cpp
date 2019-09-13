
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
void deg2rad(float * v_initial_param_7147_2887, float * & v_user_func_7149_2888, int v_N_2763){
    // Allocate memory for output pointers
    v_user_func_7149_2888 = reinterpret_cast<float *>(malloc((v_N_2763 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_2886 = 0;(v_i_2886 <= (-1 + v_N_2763)); (++v_i_2886)){
        v_user_func_7149_2888[v_i_2886] = d2r_uf(v_initial_param_7147_2887[v_i_2886]); 
    }
}
}; 