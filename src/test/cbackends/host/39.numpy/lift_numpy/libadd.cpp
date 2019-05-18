
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
void add(float * v_initial_param_591_255, float * v_initial_param_592_256, float * & v_user_func_598_258, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_598_258 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_254 = 0;(v_i_254 <= (-1 + v_N_0)); (++v_i_254)){
        v_user_func_598_258[v_i_254] = add(v_initial_param_591_255[v_i_254], v_initial_param_592_256[v_i_254]); 
    }
}
}; 