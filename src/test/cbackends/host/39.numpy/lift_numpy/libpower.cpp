
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
void power(float * v_initial_param_650_275, float * v_initial_param_651_276, float * & v_user_func_657_278, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_657_278 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_274 = 0;(v_i_274 <= (-1 + v_N_0)); (++v_i_274)){
        v_user_func_657_278[v_i_274] = power_uf(v_initial_param_650_275[v_i_274], v_initial_param_651_276[v_i_274]); 
    }
}
}; 