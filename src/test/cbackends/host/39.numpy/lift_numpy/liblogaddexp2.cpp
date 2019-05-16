
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
void logaddexp2(float * v_initial_param_495_205, float * v_initial_param_496_206, float * & v_user_func_502_208, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_502_208 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_204 = 0;(v_i_204 <= (-1 + v_N_0)); (++v_i_204)){
        v_user_func_502_208[v_i_204] = logaddexp2_uf(v_initial_param_495_205[v_i_204], v_initial_param_496_206[v_i_204]); 
    }
}
}; 