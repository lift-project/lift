
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
void trapz(float * v_initial_param_393_183, float * v_initial_param_394_184, float * & v_user_func_397_187, int v_N_0){
    // Allocate memory for output pointers
    float * v_user_func_421_186 = reinterpret_cast<float *>(malloc(((-1 + v_N_0) * sizeof(float))));
    v_user_func_397_187 = reinterpret_cast<float *>(malloc((1 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_182 = 0;(v_i_182 <= (-2 + v_N_0)); (++v_i_182)){
        v_user_func_421_186[v_i_182] = trapz(v_initial_param_393_183[v_i_182], v_initial_param_393_183[(1 + v_i_182)], v_initial_param_394_184[v_i_182], v_initial_param_394_184[(1 + v_i_182)]); 
    }
    // For each element reduced sequentially
    v_user_func_397_187[0] = 0.0f; 
    for (int v_i_181 = 0;(v_i_181 <= (-2 + v_N_0)); (++v_i_181)){
        v_user_func_397_187[0] = add(v_user_func_397_187[0], v_user_func_421_186[v_i_181]); 
    }
}
}; 