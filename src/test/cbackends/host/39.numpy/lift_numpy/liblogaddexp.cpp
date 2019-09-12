
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
void logaddexp(float * v_initial_param_7484_3016, float * v_initial_param_7485_3017, float * & v_user_func_7491_3019, int v_N_2763){
    // Allocate memory for output pointers
    v_user_func_7491_3019 = reinterpret_cast<float *>(malloc((v_N_2763 * sizeof(float)))); 
    {
        
    }
    {
        
    }
    // For each element processed sequentially
    for (int v_i_3015 = 0;(v_i_3015 <= (-1 + v_N_2763)); (++v_i_3015)){
        v_user_func_7491_3019[v_i_3015] = logaddexp_uf(v_initial_param_7484_3016[v_i_3015], v_initial_param_7485_3017[v_i_3015]); 
    }
}
}; 