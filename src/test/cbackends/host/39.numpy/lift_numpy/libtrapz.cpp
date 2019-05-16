
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
void trapz(float * v_initial_param_390_179, float * v_initial_param_391_180, float * & v_user_func_394_183, int v_N_0){
    // Allocate memory for output pointers
    float * v_user_func_418_182 = reinterpret_cast<float *>(malloc(((-1 + v_N_0) * sizeof(float))));
    v_user_func_394_183 = reinterpret_cast<float *>(malloc((1 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_178 = 0;(v_i_178 <= (-2 + v_N_0)); (++v_i_178)){
        v_user_func_418_182[v_i_178] = trapz(v_initial_param_390_179[v_i_178], v_initial_param_390_179[(1 + v_i_178)], v_initial_param_391_180[v_i_178], v_initial_param_391_180[(1 + v_i_178)]); 
    }
    // For each element reduced sequentially
    v_user_func_394_183[0] = 0.0f; 
    for (int v_i_177 = 0;(v_i_177 <= (-2 + v_N_0)); (++v_i_177)){
        v_user_func_394_183[0] = add(v_user_func_394_183[0], v_user_func_418_182[v_i_177]); 
    }
}
}; 