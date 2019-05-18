
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
void add(float * v_initial_param_588_252, float * v_initial_param_589_253, float * & v_user_func_595_255, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_595_255 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_251 = 0;(v_i_251 <= (-1 + v_N_0)); (++v_i_251)){
        v_user_func_595_255[v_i_251] = add(v_initial_param_588_252[v_i_251], v_initial_param_589_253[v_i_251]); 
    }
}
}; 