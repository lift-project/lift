
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
void trapz(float * v_initial_param_378_166, float * v_initial_param_379_167, float * & v_user_func_382_170, int v_N_0){
    // Allocate memory for output pointers
    float * v_user_func_406_169 = reinterpret_cast<float *>(malloc(((-1 + v_N_0) * sizeof(float))));
    v_user_func_382_170 = reinterpret_cast<float *>(malloc((1 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_165 = 0;(v_i_165 <= (-2 + v_N_0)); (++v_i_165)){
        v_user_func_406_169[v_i_165] = trapz(v_initial_param_378_166[v_i_165], v_initial_param_378_166[(1 + v_i_165)], v_initial_param_379_167[v_i_165], v_initial_param_379_167[(1 + v_i_165)]); 
    }
    // For each element reduced sequentially
    v_user_func_382_170[0] = 0.0f; 
    for (int v_i_164 = 0;(v_i_164 <= (-2 + v_N_0)); (++v_i_164)){
        v_user_func_382_170[0] = add(v_user_func_382_170[0], v_user_func_406_169[v_i_164]); 
    }
}
}; 