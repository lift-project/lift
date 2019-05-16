
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef LOG_UF_H
#define LOG_UF_H
; 
float log_uf(float x){
    return log(x) ;; 
}

#endif
 ; 
void lift_log(float * v_initial_param_451_186, float * & v_user_func_453_187, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_453_187 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_185 = 0;(v_i_185 <= (-1 + v_N_0)); (++v_i_185)){
        v_user_func_453_187[v_i_185] = log_uf(v_initial_param_451_186[v_i_185]); 
    }
}
}; 