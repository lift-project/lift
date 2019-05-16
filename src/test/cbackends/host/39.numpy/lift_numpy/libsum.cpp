
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
void sum(float * v_initial_param_1557_312, float * & v_user_func_1560_313, int v_N_190){
    // Allocate memory for output pointers
    v_user_func_1560_313 = reinterpret_cast<float *>(malloc((1 * sizeof(float)))); 
    // For each element reduced sequentially
    v_user_func_1560_313[0] = 0.0f; 
    for (int v_i_311 = 0;(v_i_311 <= (-1 + v_N_190)); (++v_i_311)){
        v_user_func_1560_313[0] = add(v_user_func_1560_313[0], v_initial_param_1557_312[v_i_311]); 
    }
}
}; 