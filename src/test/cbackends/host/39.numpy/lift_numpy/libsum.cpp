
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
void sum(float * v_initial_param_298_156, float * & v_user_func_301_157, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_301_157 = reinterpret_cast<float *>(malloc((1 * sizeof(float)))); 
    // For each element reduced sequentially
    v_user_func_301_157[0] = 0.0f; 
    for (int v_i_155 = 0;(v_i_155 <= (-1 + v_N_0)); (++v_i_155)){
        v_user_func_301_157[0] = add(v_user_func_301_157[0], v_initial_param_298_156[v_i_155]); 
    }
}
}; 