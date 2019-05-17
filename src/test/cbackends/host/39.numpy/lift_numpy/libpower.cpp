
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef POWER_UF_H
#define POWER_UF_H
; 
float power_uf(float x, float y){
    return pow(x, y);; 
}

#endif
 ; 
void power(float * v_initial_param_646_271, float * v_initial_param_647_272, float * & v_user_func_653_274, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_653_274 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_270 = 0;(v_i_270 <= (-1 + v_N_0)); (++v_i_270)){
        v_user_func_653_274[v_i_270] = power_uf(v_initial_param_646_271[v_i_270], v_initial_param_647_272[v_i_270]); 
    }
}
}; 