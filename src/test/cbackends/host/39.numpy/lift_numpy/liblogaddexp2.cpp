
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef LOGADDEXP2_UF_H
#define LOGADDEXP2_UF_H
; 
float logaddexp2_uf(float x1, float x2){
    { return log2(pow(2,x1) + pow(2,x2)); }; 
}

#endif
 ; 
void logaddexp2(float * v_initial_param_511_223, float * v_initial_param_512_224, float * & v_user_func_518_226, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_518_226 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_222 = 0;(v_i_222 <= (-1 + v_N_0)); (++v_i_222)){
        v_user_func_518_226[v_i_222] = logaddexp2_uf(v_initial_param_511_223[v_i_222], v_initial_param_512_224[v_i_222]); 
    }
}
}; 