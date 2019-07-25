
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef DIFF2_H
#define DIFF2_H
; 
float diff2(float l, float r){
    { return (r - l); }; 
}

#endif
 ; 
void diff(float * v_initial_param_3374_562, float * & v_user_func_3377_563, int v_N_352){
    // Allocate memory for output pointers
    v_user_func_3377_563 = reinterpret_cast<float *>(malloc(((-1 + v_N_352) * sizeof(float)))); 
    {
        
    }
    // For each element processed sequentially
    for (int v_i_560 = 0;(v_i_560 <= (-2 + v_N_352)); (++v_i_560)){
        // For each element reduced sequentially
        v_user_func_3377_563[v_i_560] = 0.0f; 
        for (int v_i_561 = 0;(v_i_561 <= 1); (++v_i_561)){
            v_user_func_3377_563[v_i_560] = diff2(v_user_func_3377_563[v_i_560], v_initial_param_3374_562[(v_i_560 + v_i_561)]); 
        }
    }
}
}; 