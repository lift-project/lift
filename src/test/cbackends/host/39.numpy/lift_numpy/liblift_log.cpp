
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
void lift_log(float * v_initial_param_456_191, float * & v_user_func_458_192, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_458_192 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_190 = 0;(v_i_190 <= (-1 + v_N_0)); (++v_i_190)){
        v_user_func_458_192[v_i_190] = log_uf(v_initial_param_456_191[v_i_190]); 
    }
}
}; 