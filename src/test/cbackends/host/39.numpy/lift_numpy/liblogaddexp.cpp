
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef LOGADDEXP_UF_H
#define LOGADDEXP_UF_H
; 
float logaddexp_uf(float x1, float x2){
    { return log(exp(x1) + exp(x2)); }; 
}

#endif
 ; 
void logaddexp(float * v_initial_param_493_214, float * v_initial_param_494_215, float * & v_user_func_500_217, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_500_217 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_213 = 0;(v_i_213 <= (-1 + v_N_0)); (++v_i_213)){
        v_user_func_500_217[v_i_213] = logaddexp_uf(v_initial_param_493_214[v_i_213], v_initial_param_494_215[v_i_213]); 
    }
}
}; 