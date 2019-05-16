
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
void logaddexp(float * v_initial_param_485_204, float * v_initial_param_486_205, float * & v_user_func_492_207, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_492_207 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_203 = 0;(v_i_203 <= (-1 + v_N_0)); (++v_i_203)){
        v_user_func_492_207[v_i_203] = logaddexp_uf(v_initial_param_485_204[v_i_203], v_initial_param_486_205[v_i_203]); 
    }
}
}; 