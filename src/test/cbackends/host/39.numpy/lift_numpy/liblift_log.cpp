
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
void lift_log(float * v_initial_param_460_197, float * & v_user_func_462_198, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_462_198 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_196 = 0;(v_i_196 <= (-1 + v_N_0)); (++v_i_196)){
        v_user_func_462_198[v_i_196] = log_uf(v_initial_param_460_197[v_i_196]); 
    }
}
}; 