
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
void logaddexp(float * v_initial_param_479_198, float * v_initial_param_480_199, float * & v_user_func_486_201, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_486_201 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_197 = 0;(v_i_197 <= (-1 + v_N_0)); (++v_i_197)){
        v_user_func_486_201[v_i_197] = logaddexp_uf(v_initial_param_479_198[v_i_197], v_initial_param_480_199[v_i_197]); 
    }
}
}; 