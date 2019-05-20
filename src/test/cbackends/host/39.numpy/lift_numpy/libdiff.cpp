
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
void diff(float * v_initial_param_378_212, float * & v_user_func_381_213, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_381_213 = reinterpret_cast<float *>(malloc(((-1 + v_N_0) * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_210 = 0;(v_i_210 <= (-2 + v_N_0)); (++v_i_210)){
        // For each element reduced sequentially
        v_user_func_381_213[v_i_210] = 0.0f; 
        for (int v_i_211 = 0;(v_i_211 <= 1); (++v_i_211)){
            v_user_func_381_213[v_i_210] = diff2(v_user_func_381_213[v_i_210], v_initial_param_378_212[(v_i_210 + v_i_211)]); 
        }
    }
}
}; 