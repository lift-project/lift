
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
void subtract(float * v_initial_param_661_277, float * v_initial_param_662_278, float * & v_user_func_668_280, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_668_280 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_276 = 0;(v_i_276 <= (-1 + v_N_0)); (++v_i_276)){
        v_user_func_668_280[v_i_276] = subtract(v_initial_param_661_277[v_i_276], v_initial_param_662_278[v_i_276]); 
    }
}
}; 