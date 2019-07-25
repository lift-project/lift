
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
void subtract(float * v_initial_param_3708_668, float * v_initial_param_3709_669, float * & v_user_func_3715_671, int v_N_352){
    // Allocate memory for output pointers
    v_user_func_3715_671 = reinterpret_cast<float *>(malloc((v_N_352 * sizeof(float)))); 
    {
        
    }
    {
        
    }
    // For each element processed sequentially
    for (int v_i_667 = 0;(v_i_667 <= (-1 + v_N_352)); (++v_i_667)){
        v_user_func_3715_671[v_i_667] = subtract(v_initial_param_3708_668[v_i_667], v_initial_param_3709_669[v_i_667]); 
    }
}
}; 