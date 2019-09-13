
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef RECIPROCAL_UF_H
#define RECIPROCAL_UF_H
; 
float reciprocal_uf(float x){
    return 1.0f/x; 
}

#endif
 ; 
void reciprocal(float * v_initial_param_7589_3055, float * & v_user_func_7591_3056, int v_N_2763){
    // Allocate memory for output pointers
    v_user_func_7591_3056 = reinterpret_cast<float *>(malloc((v_N_2763 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_3054 = 0;(v_i_3054 <= (-1 + v_N_2763)); (++v_i_3054)){
        v_user_func_7591_3056[v_i_3054] = reciprocal_uf(v_initial_param_7589_3055[v_i_3054]); 
    }
}
}; 