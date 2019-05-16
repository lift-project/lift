
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
void trapz(float * v_initial_param_375_163, float * v_initial_param_376_164, float * & v_user_func_379_167, int v_N_0){
    // Allocate memory for output pointers
    float * v_user_func_403_166 = reinterpret_cast<float *>(malloc(((-1 + v_N_0) * sizeof(float))));
    v_user_func_379_167 = reinterpret_cast<float *>(malloc((1 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_162 = 0;(v_i_162 <= (-2 + v_N_0)); (++v_i_162)){
        v_user_func_403_166[v_i_162] = trapz(v_initial_param_375_163[v_i_162], v_initial_param_375_163[(1 + v_i_162)], v_initial_param_376_164[v_i_162], v_initial_param_376_164[(1 + v_i_162)]); 
    }
    // For each element reduced sequentially
    v_user_func_379_167[0] = 0.0f; 
    for (int v_i_161 = 0;(v_i_161 <= (-2 + v_N_0)); (++v_i_161)){
        v_user_func_379_167[0] = add(v_user_func_379_167[0], v_user_func_403_166[v_i_161]); 
    }
}
}; 