
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
void sum_axis_1(float * v_initial_param_3337_540, float * & v_user_func_3340_541, int v_N_352, int v_M_353){
    // Allocate memory for output pointers
    v_user_func_3340_541 = reinterpret_cast<float *>(malloc((v_M_353 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_538 = 0;(v_i_538 <= (-1 + v_M_353)); (++v_i_538)){
        // For each element reduced sequentially
        v_user_func_3340_541[v_i_538] = 0.0f; 
        for (int v_i_539 = 0;(v_i_539 <= (-1 + v_N_352)); (++v_i_539)){
            v_user_func_3340_541[v_i_538] = add(v_user_func_3340_541[v_i_538], v_initial_param_3337_540[(v_i_539 + (v_N_352 * v_i_538))]); 
        }
    }
}
}; 