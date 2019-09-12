
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
void lift_log(float * v_initial_param_7456_3004, float * & v_user_func_7458_3005, int v_N_2763){
    // Allocate memory for output pointers
    v_user_func_7458_3005 = reinterpret_cast<float *>(malloc((v_N_2763 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_3003 = 0;(v_i_3003 <= (-1 + v_N_2763)); (++v_i_3003)){
        v_user_func_7458_3005[v_i_3003] = log_uf(v_initial_param_7456_3004[v_i_3003]); 
    }
}
}; 