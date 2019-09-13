
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
void ediff1d(float * v_initial_param_7318_2977, float * & v_user_func_7321_2978, int v_N_2763){
    // Allocate memory for output pointers
    v_user_func_7321_2978 = reinterpret_cast<float *>(malloc(((-1 + v_N_2763) * sizeof(float)))); 
    {
        
    }
    // For each element processed sequentially
    for (int v_i_2975 = 0;(v_i_2975 <= (-2 + v_N_2763)); (++v_i_2975)){
        // For each element reduced sequentially
        v_user_func_7321_2978[v_i_2975] = 0.0f; 
        for (int v_i_2976 = 0;(v_i_2976 <= 1); (++v_i_2976)){
            v_user_func_7321_2978[v_i_2975] = diff2(v_user_func_7321_2978[v_i_2975], v_initial_param_7318_2977[(v_i_2975 + v_i_2976)]); 
        }
    }
}
}; 