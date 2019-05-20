
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef SUBTRACT_H
#define SUBTRACT_H
; 
float subtract(float l, float r){
    { return l - r; }; 
}

#endif
 ; 
void subtract(float * v_initial_param_699_312, float * v_initial_param_700_313, float * & v_user_func_706_315, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_706_315 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_311 = 0;(v_i_311 <= (-1 + v_N_0)); (++v_i_311)){
        v_user_func_706_315[v_i_311] = subtract(v_initial_param_699_312[v_i_311], v_initial_param_700_313[v_i_311]); 
    }
}
}; 