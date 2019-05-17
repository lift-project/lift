
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
void trapz(float * v_initial_param_392_182, float * v_initial_param_393_183, float * & v_user_func_396_186, int v_N_0){
    // Allocate memory for output pointers
    float * v_user_func_420_185 = reinterpret_cast<float *>(malloc(((-1 + v_N_0) * sizeof(float))));
    v_user_func_396_186 = reinterpret_cast<float *>(malloc((1 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_181 = 0;(v_i_181 <= (-2 + v_N_0)); (++v_i_181)){
        v_user_func_420_185[v_i_181] = trapz(v_initial_param_392_182[v_i_181], v_initial_param_392_182[(1 + v_i_181)], v_initial_param_393_183[v_i_181], v_initial_param_393_183[(1 + v_i_181)]); 
    }
    // For each element reduced sequentially
    v_user_func_396_186[0] = 0.0f; 
    for (int v_i_180 = 0;(v_i_180 <= (-2 + v_N_0)); (++v_i_180)){
        v_user_func_396_186[0] = add(v_user_func_396_186[0], v_user_func_420_185[v_i_180]); 
    }
}
}; 