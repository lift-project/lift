
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef LOG10_UF_H
#define LOG10_UF_H
; 
float log10_uf(float x){
    return log10(x) ;; 
}

#endif
 ; 
void lift_log10(float * v_initial_param_456_187, float * & v_user_func_458_188, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_458_188 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_186 = 0;(v_i_186 <= (-1 + v_N_0)); (++v_i_186)){
        v_user_func_458_188[v_i_186] = log10_uf(v_initial_param_456_187[v_i_186]); 
    }
}
}; 