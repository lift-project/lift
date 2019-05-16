
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
void lift_log(float * v_initial_param_454_189, float * & v_user_func_456_190, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_456_190 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_188 = 0;(v_i_188 <= (-1 + v_N_0)); (++v_i_188)){
        v_user_func_456_190[v_i_188] = log_uf(v_initial_param_454_189[v_i_188]); 
    }
}
}; 