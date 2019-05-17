
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
void sum(float * v_initial_param_291_149, float * & v_user_func_294_150, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_294_150 = reinterpret_cast<float *>(malloc((1 * sizeof(float)))); 
    // For each element reduced sequentially
    v_user_func_294_150[0] = 0.0f; 
    for (int v_i_148 = 0;(v_i_148 <= (-1 + v_N_0)); (++v_i_148)){
        v_user_func_294_150[0] = add(v_user_func_294_150[0], v_initial_param_291_149[v_i_148]); 
    }
}
}; 