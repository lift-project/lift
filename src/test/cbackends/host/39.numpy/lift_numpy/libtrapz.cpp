
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
void trapz(float * v_initial_param_377_165, float * v_initial_param_378_166, float * & v_user_func_381_169, int v_N_0){
    // Allocate memory for output pointers
    float * v_user_func_405_168 = reinterpret_cast<float *>(malloc(((-1 + v_N_0) * sizeof(float))));
    v_user_func_381_169 = reinterpret_cast<float *>(malloc((1 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_164 = 0;(v_i_164 <= (-2 + v_N_0)); (++v_i_164)){
        v_user_func_405_168[v_i_164] = trapz(v_initial_param_377_165[v_i_164], v_initial_param_377_165[(1 + v_i_164)], v_initial_param_378_166[v_i_164], v_initial_param_378_166[(1 + v_i_164)]); 
    }
    // For each element reduced sequentially
    v_user_func_381_169[0] = 0.0f; 
    for (int v_i_163 = 0;(v_i_163 <= (-2 + v_N_0)); (++v_i_163)){
        v_user_func_381_169[0] = add(v_user_func_381_169[0], v_user_func_405_168[v_i_163]); 
    }
}
}; 