
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
void lift_log(float * v_initial_param_503_237, float * & v_user_func_505_238, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_505_238 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_236 = 0;(v_i_236 <= (-1 + v_N_0)); (++v_i_236)){
        v_user_func_505_238[v_i_236] = log_uf(v_initial_param_503_237[v_i_236]); 
    }
}
}; 