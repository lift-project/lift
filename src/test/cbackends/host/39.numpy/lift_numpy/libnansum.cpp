
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
void nansum(float * v_initial_param_298_162, float * & v_user_func_301_163, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_301_163 = reinterpret_cast<float *>(malloc((1 * sizeof(float)))); 
    // For each element reduced sequentially
    v_user_func_301_163[0] = 0.0f; 
    for (int v_i_161 = 0;(v_i_161 <= (-1 + v_N_0)); (++v_i_161)){
        v_user_func_301_163[0] = add(v_user_func_301_163[0], v_initial_param_298_162[v_i_161]); 
    }
}
}; 