
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
void sum_axis_1(float * v_initial_param_7281_2951, float * & v_user_func_7284_2952, int v_N_2763, int v_M_2764){
    // Allocate memory for output pointers
    v_user_func_7284_2952 = reinterpret_cast<float *>(malloc((v_M_2764 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_2949 = 0;(v_i_2949 <= (-1 + v_M_2764)); (++v_i_2949)){
        // For each element reduced sequentially
        v_user_func_7284_2952[v_i_2949] = 0.0f; 
        for (int v_i_2950 = 0;(v_i_2950 <= (-1 + v_N_2763)); (++v_i_2950)){
            v_user_func_7284_2952[v_i_2949] = add(v_user_func_7284_2952[v_i_2949], v_initial_param_7281_2951[(v_i_2950 + (v_N_2763 * v_i_2949))]); 
        }
    }
}
}; 