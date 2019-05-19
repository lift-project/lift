
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
void logaddexp2(float * v_initial_param_526_238, float * v_initial_param_527_239, float * & v_user_func_533_241, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_533_241 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_237 = 0;(v_i_237 <= (-1 + v_N_0)); (++v_i_237)){
        v_user_func_533_241[v_i_237] = logaddexp2_uf(v_initial_param_526_238[v_i_237], v_initial_param_527_239[v_i_237]); 
    }
}
}; 