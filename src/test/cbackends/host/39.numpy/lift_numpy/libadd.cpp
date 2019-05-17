
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef ADD_H
#define ADD_H
; 
float add(float l, float r){
    { return (l + r); }; 
}

#endif
 ; 
void add(float * v_initial_param_587_251, float * v_initial_param_588_252, float * & v_user_func_594_254, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_594_254 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_250 = 0;(v_i_250 <= (-1 + v_N_0)); (++v_i_250)){
        v_user_func_594_254[v_i_250] = add(v_initial_param_587_251[v_i_250], v_initial_param_588_252[v_i_250]); 
    }
}
}; 