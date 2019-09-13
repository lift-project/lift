
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
void radians(float * v_initial_param_7147_2884, float * & v_user_func_7149_2885, int v_N_2763){
    // Allocate memory for output pointers
    v_user_func_7149_2885 = reinterpret_cast<float *>(malloc((v_N_2763 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_2883 = 0;(v_i_2883 <= (-1 + v_N_2763)); (++v_i_2883)){
        v_user_func_7149_2885[v_i_2883] = d2r_uf(v_initial_param_7147_2884[v_i_2883]); 
    }
}
}; 