
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
void lift_log(float * v_initial_param_447_182, float * & v_user_func_449_183, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_449_183 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_181 = 0;(v_i_181 <= (-1 + v_N_0)); (++v_i_181)){
        v_user_func_449_183[v_i_181] = log_uf(v_initial_param_447_182[v_i_181]); 
    }
}
}; 