
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
void lift_log10(float * v_initial_param_460_191, float * & v_user_func_462_192, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_462_192 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_190 = 0;(v_i_190 <= (-1 + v_N_0)); (++v_i_190)){
        v_user_func_462_192[v_i_190] = log10_uf(v_initial_param_460_191[v_i_190]); 
    }
}
}; 