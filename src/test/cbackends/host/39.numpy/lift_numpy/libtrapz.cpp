
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
void trapz(float * v_initial_param_448_228, float * v_initial_param_449_229, float * & v_user_func_452_232, int v_N_0){
    // Allocate memory for output pointers
    float * v_user_func_476_231 = reinterpret_cast<float *>(malloc(((-1 + v_N_0) * sizeof(float))));
    v_user_func_452_232 = reinterpret_cast<float *>(malloc((1 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_227 = 0;(v_i_227 <= (-2 + v_N_0)); (++v_i_227)){
        v_user_func_476_231[v_i_227] = trapz(v_initial_param_448_228[v_i_227], v_initial_param_448_228[(1 + v_i_227)], v_initial_param_449_229[v_i_227], v_initial_param_449_229[(1 + v_i_227)]); 
    }
    // For each element reduced sequentially
    v_user_func_452_232[0] = 0.0f; 
    for (int v_i_226 = 0;(v_i_226 <= (-2 + v_N_0)); (++v_i_226)){
        v_user_func_452_232[0] = add(v_user_func_452_232[0], v_user_func_476_231[v_i_226]); 
    }
}
}; 