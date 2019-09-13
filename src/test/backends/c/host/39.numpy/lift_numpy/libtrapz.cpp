
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef TRAPZ_H
#define TRAPZ_H
; 
float trapz(float x1, float x2, float y1, float y2){
    { return (x2-x1)*(y2+y1)/2.0f; }; 
}

#endif
 ; 
#ifndef ADD_H
#define ADD_H
; 
float add(float l, float r){
    { return (l + r); }; 
}

#endif
 ; 
void trapz(float * v_initial_param_7388_2989, float * v_initial_param_7389_2990, float * & v_user_func_7392_2993, int v_N_2763){
    // Allocate memory for output pointers
    float * v_user_func_7416_2992 = reinterpret_cast<float *>(malloc(((-1 + v_N_2763) * sizeof(float))));
    v_user_func_7392_2993 = reinterpret_cast<float *>(malloc((1 * sizeof(float)))); 
    {
        {
            
        }
        {
            
        }
    }
    // For each element processed sequentially
    for (int v_i_2988 = 0;(v_i_2988 <= (-2 + v_N_2763)); (++v_i_2988)){
        v_user_func_7416_2992[v_i_2988] = trapz(v_initial_param_7388_2989[v_i_2988], v_initial_param_7388_2989[(1 + v_i_2988)], v_initial_param_7389_2990[v_i_2988], v_initial_param_7389_2990[(1 + v_i_2988)]); 
    }
    // For each element reduced sequentially
    v_user_func_7392_2993[0] = 0.0f; 
    for (int v_i_2987 = 0;(v_i_2987 <= (-2 + v_N_2763)); (++v_i_2987)){
        v_user_func_7392_2993[0] = add(v_user_func_7392_2993[0], v_user_func_7416_2992[v_i_2987]); 
    }
}
}; 