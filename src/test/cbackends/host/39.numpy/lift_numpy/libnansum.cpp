
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
void nansum(float * v_initial_param_1557_318, float * & v_user_func_1560_319, int v_N_190){
    // Allocate memory for output pointers
    v_user_func_1560_319 = reinterpret_cast<float *>(malloc((1 * sizeof(float)))); 
    // For each element reduced sequentially
    v_user_func_1560_319[0] = 0.0f; 
    for (int v_i_317 = 0;(v_i_317 <= (-1 + v_N_190)); (++v_i_317)){
        v_user_func_1560_319[0] = add(v_user_func_1560_319[0], v_initial_param_1557_318[v_i_317]); 
    }
}
}; 