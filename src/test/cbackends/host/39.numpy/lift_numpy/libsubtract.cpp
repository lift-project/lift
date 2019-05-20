
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
void subtract(float * v_initial_param_712_318, float * v_initial_param_713_319, float * & v_user_func_719_321, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_719_321 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_317 = 0;(v_i_317 <= (-1 + v_N_0)); (++v_i_317)){
        v_user_func_719_321[v_i_317] = subtract(v_initial_param_712_318[v_i_317], v_initial_param_713_319[v_i_317]); 
    }
}
}; 