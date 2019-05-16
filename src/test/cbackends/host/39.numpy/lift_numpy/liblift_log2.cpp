
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
void lift_log2(float * v_initial_param_471_198, float * & v_user_func_473_199, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_473_199 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_197 = 0;(v_i_197 <= (-1 + v_N_0)); (++v_i_197)){
        v_user_func_473_199[v_i_197] = log2_uf(v_initial_param_471_198[v_i_197]); 
    }
}
}; 