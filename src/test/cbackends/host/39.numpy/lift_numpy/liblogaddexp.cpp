
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
void logaddexp(float * v_initial_param_3540_605, float * v_initial_param_3541_606, float * & v_user_func_3547_608, int v_N_352){
    // Allocate memory for output pointers
    v_user_func_3547_608 = reinterpret_cast<float *>(malloc((v_N_352 * sizeof(float)))); 
    {
        
    }
    {
        
    }
    // For each element processed sequentially
    for (int v_i_604 = 0;(v_i_604 <= (-1 + v_N_352)); (++v_i_604)){
        v_user_func_3547_608[v_i_604] = logaddexp_uf(v_initial_param_3540_605[v_i_604], v_initial_param_3541_606[v_i_604]); 
    }
}
}; 