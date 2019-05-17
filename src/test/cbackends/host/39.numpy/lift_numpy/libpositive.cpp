
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef ID_H
#define ID_H
; 
float id(float x){
    { return x; }; 
}

#endif
 ; 
void positive(float * v_initial_param_600_251, float * & v_user_func_602_252, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_602_252 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_250 = 0;(v_i_250 <= (-1 + v_N_0)); (++v_i_250)){
        v_user_func_602_252[v_i_250] = id(v_initial_param_600_251[v_i_250]); 
    }
}
}; 