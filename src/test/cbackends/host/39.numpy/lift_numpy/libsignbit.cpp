
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef SIGNBIT_UF_H
#define SIGNBIT_UF_H
; 
float signbit_uf(float x){
    return x<0? 1:0 ;; 
}

#endif
 ; 
void signbit(float * v_initial_param_542_227, float * & v_user_func_544_228, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_544_228 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_226 = 0;(v_i_226 <= (-1 + v_N_0)); (++v_i_226)){
        v_user_func_544_228[v_i_226] = signbit_uf(v_initial_param_542_227[v_i_226]); 
    }
}
}; 