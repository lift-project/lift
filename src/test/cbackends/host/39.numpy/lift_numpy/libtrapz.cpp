
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
void trapz(float * v_initial_param_389_177, float * v_initial_param_390_178, float * & v_user_func_393_181, int v_N_0){
    // Allocate memory for output pointers
    float * v_user_func_417_180 = reinterpret_cast<float *>(malloc(((-1 + v_N_0) * sizeof(float))));
    v_user_func_393_181 = reinterpret_cast<float *>(malloc((1 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_176 = 0;(v_i_176 <= (-2 + v_N_0)); (++v_i_176)){
        v_user_func_417_180[v_i_176] = trapz(v_initial_param_389_177[v_i_176], v_initial_param_389_177[(1 + v_i_176)], v_initial_param_390_178[v_i_176], v_initial_param_390_178[(1 + v_i_176)]); 
    }
    // For each element reduced sequentially
    v_user_func_393_181[0] = 0.0f; 
    for (int v_i_175 = 0;(v_i_175 <= (-2 + v_N_0)); (++v_i_175)){
        v_user_func_393_181[0] = add(v_user_func_393_181[0], v_user_func_417_180[v_i_175]); 
    }
}
}; 