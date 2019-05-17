
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
void sum(float * v_initial_param_292_150, float * & v_user_func_295_151, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_295_151 = reinterpret_cast<float *>(malloc((1 * sizeof(float)))); 
    // For each element reduced sequentially
    v_user_func_295_151[0] = 0.0f; 
    for (int v_i_149 = 0;(v_i_149 <= (-1 + v_N_0)); (++v_i_149)){
        v_user_func_295_151[0] = add(v_user_func_295_151[0], v_initial_param_292_150[v_i_149]); 
    }
}
}; 