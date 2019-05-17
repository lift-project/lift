
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
void lift_log(float * v_initial_param_462_199, float * & v_user_func_464_200, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_464_200 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_198 = 0;(v_i_198 <= (-1 + v_N_0)); (++v_i_198)){
        v_user_func_464_200[v_i_198] = log_uf(v_initial_param_462_199[v_i_198]); 
    }
}
}; 