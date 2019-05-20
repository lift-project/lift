
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
void sum(float * v_initial_param_310_172, float * & v_user_func_313_173, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_313_173 = reinterpret_cast<float *>(malloc((1 * sizeof(float)))); 
    // For each element reduced sequentially
    v_user_func_313_173[0] = 0.0f; 
    for (int v_i_171 = 0;(v_i_171 <= (-1 + v_N_0)); (++v_i_171)){
        v_user_func_313_173[0] = add(v_user_func_313_173[0], v_initial_param_310_172[v_i_171]); 
    }
}
}; 