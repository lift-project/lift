
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
void lift_log10(float * v_initial_param_459_190, float * & v_user_func_461_191, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_461_191 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_189 = 0;(v_i_189 <= (-1 + v_N_0)); (++v_i_189)){
        v_user_func_461_191[v_i_189] = log10_uf(v_initial_param_459_190[v_i_189]); 
    }
}
}; 