
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
void ediff1d(float * v_initial_param_325_173, float * & v_user_func_328_174, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_328_174 = reinterpret_cast<float *>(malloc(((-1 + v_N_0) * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_171 = 0;(v_i_171 <= (-2 + v_N_0)); (++v_i_171)){
        // For each element reduced sequentially
        v_user_func_328_174[v_i_171] = 0.0f; 
        for (int v_i_172 = 0;(v_i_172 <= 1); (++v_i_172)){
            v_user_func_328_174[v_i_171] = diff2(v_user_func_328_174[v_i_171], v_initial_param_325_173[(v_i_171 + v_i_172)]); 
        }
    }
}
}; 