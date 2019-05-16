
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
void lift_log(float * v_initial_param_457_192, float * & v_user_func_459_193, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_459_193 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_191 = 0;(v_i_191 <= (-1 + v_N_0)); (++v_i_191)){
        v_user_func_459_193[v_i_191] = log_uf(v_initial_param_457_192[v_i_191]); 
    }
}
}; 