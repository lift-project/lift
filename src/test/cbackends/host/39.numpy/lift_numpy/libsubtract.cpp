
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
void subtract(float * v_initial_param_668_284, float * v_initial_param_669_285, float * & v_user_func_675_287, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_675_287 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_283 = 0;(v_i_283 <= (-1 + v_N_0)); (++v_i_283)){
        v_user_func_675_287[v_i_283] = subtract(v_initial_param_668_284[v_i_283], v_initial_param_669_285[v_i_283]); 
    }
}
}; 