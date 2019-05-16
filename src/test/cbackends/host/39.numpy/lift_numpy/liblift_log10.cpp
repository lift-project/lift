
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
void lift_log10(float * v_initial_param_461_192, float * & v_user_func_463_193, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_463_193 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_191 = 0;(v_i_191 <= (-1 + v_N_0)); (++v_i_191)){
        v_user_func_463_193[v_i_191] = log10_uf(v_initial_param_461_192[v_i_191]); 
    }
}
}; 