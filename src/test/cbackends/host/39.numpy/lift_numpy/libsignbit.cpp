
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
void signbit(float * v_initial_param_537_222, float * & v_user_func_539_223, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_539_223 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_221 = 0;(v_i_221 <= (-1 + v_N_0)); (++v_i_221)){
        v_user_func_539_223[v_i_221] = signbit_uf(v_initial_param_537_222[v_i_221]); 
    }
}
}; 