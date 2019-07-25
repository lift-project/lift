
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef MULTANDSUMUP_H
#define MULTANDSUMUP_H
; 
float multAndSumUp(float acc, float l, float r){
    { return acc + (l * r); }; 
}

#endif
 ; 
void convolve(float * v_initial_param_3840_727, float * v_initial_param_3841_728, float * & v_user_func_3849_730, int v_M_353, int v_N_352){
    // Allocate memory for output pointers
    v_user_func_3849_730 = reinterpret_cast<float *>(malloc(((1 + v_M_353 + (-1 * v_N_352)) * sizeof(float)))); 
    {
        
    }
    // For each element processed sequentially
    for (int v_i_725 = 0;(v_i_725 <= (v_M_353 + (-1 * v_N_352))); (++v_i_725)){
        {
            
        }
        {
            
        }
        // For each element reduced sequentially
        v_user_func_3849_730[v_i_725] = 0.0f; 
        for (int v_i_726 = 0;(v_i_726 <= (-1 + v_N_352)); (++v_i_726)){
            v_user_func_3849_730[v_i_725] = multAndSumUp(v_user_func_3849_730[v_i_725], v_initial_param_3840_727[(v_i_725 + v_i_726)], v_initial_param_3841_728[v_i_726]); 
        }
    }
}
}; 