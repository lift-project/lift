
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
void add(float * v_initial_param_585_249, float * v_initial_param_586_250, float * & v_user_func_592_252, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_592_252 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_248 = 0;(v_i_248 <= (-1 + v_N_0)); (++v_i_248)){
        v_user_func_592_252[v_i_248] = add(v_initial_param_585_249[v_i_248], v_initial_param_586_250[v_i_248]); 
    }
}
}; 