
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef LOG2_UF_H
#define LOG2_UF_H
; 
float log2_uf(float x){
    return log2(x) ;; 
}

#endif
 ; 
void lift_log2(float * v_initial_param_487_218, float * & v_user_func_489_219, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_489_219 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_217 = 0;(v_i_217 <= (-1 + v_N_0)); (++v_i_217)){
        v_user_func_489_219[v_i_217] = log2_uf(v_initial_param_487_218[v_i_217]); 
    }
}
}; 