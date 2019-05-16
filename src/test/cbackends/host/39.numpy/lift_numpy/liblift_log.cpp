
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
void lift_log(float * v_initial_param_459_196, float * & v_user_func_461_197, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_461_197 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_195 = 0;(v_i_195 <= (-1 + v_N_0)); (++v_i_195)){
        v_user_func_461_197[v_i_195] = log_uf(v_initial_param_459_196[v_i_195]); 
    }
}
}; 