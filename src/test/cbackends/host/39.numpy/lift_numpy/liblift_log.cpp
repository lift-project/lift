
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
void lift_log(float * v_initial_param_457_193, float * & v_user_func_459_194, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_459_194 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_192 = 0;(v_i_192 <= (-1 + v_N_0)); (++v_i_192)){
        v_user_func_459_194[v_i_192] = log_uf(v_initial_param_457_193[v_i_192]); 
    }
}
}; 