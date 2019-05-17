
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
void lift_log10(float * v_initial_param_472_205, float * & v_user_func_474_206, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_474_206 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_204 = 0;(v_i_204 <= (-1 + v_N_0)); (++v_i_204)){
        v_user_func_474_206[v_i_204] = log10_uf(v_initial_param_472_205[v_i_204]); 
    }
}
}; 