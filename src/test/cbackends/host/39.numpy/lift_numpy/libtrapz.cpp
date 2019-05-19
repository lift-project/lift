
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
void trapz(float * v_initial_param_416_208, float * v_initial_param_417_209, float * & v_user_func_420_212, int v_N_0){
    // Allocate memory for output pointers
    float * v_user_func_444_211 = reinterpret_cast<float *>(malloc(((-1 + v_N_0) * sizeof(float))));
    v_user_func_420_212 = reinterpret_cast<float *>(malloc((1 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_207 = 0;(v_i_207 <= (-2 + v_N_0)); (++v_i_207)){
        v_user_func_444_211[v_i_207] = trapz(v_initial_param_416_208[v_i_207], v_initial_param_416_208[(1 + v_i_207)], v_initial_param_417_209[v_i_207], v_initial_param_417_209[(1 + v_i_207)]); 
    }
    // For each element reduced sequentially
    v_user_func_420_212[0] = 0.0f; 
    for (int v_i_206 = 0;(v_i_206 <= (-2 + v_N_0)); (++v_i_206)){
        v_user_func_420_212[0] = add(v_user_func_420_212[0], v_user_func_444_211[v_i_206]); 
    }
}
}; 