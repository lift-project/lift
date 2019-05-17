
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
void power(float * v_initial_param_644_269, float * v_initial_param_645_270, float * & v_user_func_651_272, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_651_272 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_268 = 0;(v_i_268 <= (-1 + v_N_0)); (++v_i_268)){
        v_user_func_651_272[v_i_268] = power_uf(v_initial_param_644_269[v_i_268], v_initial_param_645_270[v_i_268]); 
    }
}
}; 