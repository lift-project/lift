
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
void sum_axis_0(float * v_initial_param_7262_2939, float * & v_user_func_7265_2940, int v_N_2763, int v_M_2764){
    // Allocate memory for output pointers
    v_user_func_7265_2940 = reinterpret_cast<float *>(malloc((v_N_2763 * sizeof(float)))); 
    {
        
    }
    // For each element processed sequentially
    for (int v_i_2937 = 0;(v_i_2937 <= (-1 + v_N_2763)); (++v_i_2937)){
        // For each element reduced sequentially
        v_user_func_7265_2940[v_i_2937] = 0.0f; 
        for (int v_i_2938 = 0;(v_i_2938 <= (-1 + v_M_2764)); (++v_i_2938)){
            v_user_func_7265_2940[v_i_2937] = add(v_user_func_7265_2940[v_i_2937], v_initial_param_7262_2939[(v_i_2937 + (v_N_2763 * v_i_2938))]); 
        }
    }
}
}; 