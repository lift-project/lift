
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
void logaddexp2(float * v_initial_param_499_210, float * v_initial_param_500_211, float * & v_user_func_506_213, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_506_213 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_209 = 0;(v_i_209 <= (-1 + v_N_0)); (++v_i_209)){
        v_user_func_506_213[v_i_209] = logaddexp2_uf(v_initial_param_499_210[v_i_209], v_initial_param_500_211[v_i_209]); 
    }
}
}; 