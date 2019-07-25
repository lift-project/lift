
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
void lift_log(float * v_initial_param_3512_593, float * & v_user_func_3514_594, int v_N_352){
    // Allocate memory for output pointers
    v_user_func_3514_594 = reinterpret_cast<float *>(malloc((v_N_352 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_592 = 0;(v_i_592 <= (-1 + v_N_352)); (++v_i_592)){
        v_user_func_3514_594[v_i_592] = log_uf(v_initial_param_3512_593[v_i_592]); 
    }
}
}; 