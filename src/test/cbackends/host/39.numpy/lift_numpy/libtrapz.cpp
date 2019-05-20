
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
void trapz(float * v_initial_param_435_222, float * v_initial_param_436_223, float * & v_user_func_439_226, int v_N_0){
    // Allocate memory for output pointers
    float * v_user_func_463_225 = reinterpret_cast<float *>(malloc(((-1 + v_N_0) * sizeof(float))));
    v_user_func_439_226 = reinterpret_cast<float *>(malloc((1 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_221 = 0;(v_i_221 <= (-2 + v_N_0)); (++v_i_221)){
        v_user_func_463_225[v_i_221] = trapz(v_initial_param_435_222[v_i_221], v_initial_param_435_222[(1 + v_i_221)], v_initial_param_436_223[v_i_221], v_initial_param_436_223[(1 + v_i_221)]); 
    }
    // For each element reduced sequentially
    v_user_func_439_226[0] = 0.0f; 
    for (int v_i_220 = 0;(v_i_220 <= (-2 + v_N_0)); (++v_i_220)){
        v_user_func_439_226[0] = add(v_user_func_439_226[0], v_user_func_463_225[v_i_220]); 
    }
}
}; 