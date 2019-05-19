
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef SUBTRACT_H
#define SUBTRACT_H
; 
float subtract(float l, float r){
    { return l - r; }; 
}

#endif
 ; 
void subtract(float * v_initial_param_680_298, float * v_initial_param_681_299, float * & v_user_func_687_301, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_687_301 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_297 = 0;(v_i_297 <= (-1 + v_N_0)); (++v_i_297)){
        v_user_func_687_301[v_i_297] = subtract(v_initial_param_680_298[v_i_297], v_initial_param_681_299[v_i_297]); 
    }
}
}; 