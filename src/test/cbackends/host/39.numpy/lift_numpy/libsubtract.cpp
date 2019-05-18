
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
void subtract(float * v_initial_param_669_287, float * v_initial_param_670_288, float * & v_user_func_676_290, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_676_290 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_286 = 0;(v_i_286 <= (-1 + v_N_0)); (++v_i_286)){
        v_user_func_676_290[v_i_286] = subtract(v_initial_param_669_287[v_i_286], v_initial_param_670_288[v_i_286]); 
    }
}
}; 