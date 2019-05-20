
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef INTERP_UF_H
#define INTERP_UF_H
; 
float interp_uf(float acc, float x, float x1, float y1, float x2, float y2){
    if (x >= x1 && x <= x2) {auto a = (y1-y2)/(x1-x2); auto b = y1 - a*x1; return acc + a*x + b;} else return acc + 0.0f;; 
}

#endif
 ; 
void interp(float * v_initial_param_951_415, float * v_initial_param_952_416, float * v_initial_param_953_417, float * & v_user_func_973_419, int v_N_0, int v_M_1){
    // Allocate memory for output pointers
    v_user_func_973_419 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_413 = 0;(v_i_413 <= (-1 + v_N_0)); (++v_i_413)){
        // For each element reduced sequentially
        v_user_func_973_419[v_i_413] = 0.0f; 
        for (int v_i_414 = 0;(v_i_414 <= (-2 + v_M_1)); (++v_i_414)){
            v_user_func_973_419[v_i_413] = interp_uf(v_user_func_973_419[v_i_413], v_initial_param_951_415[v_i_413], v_initial_param_952_416[v_i_414], v_initial_param_953_417[v_i_414], v_initial_param_952_416[(1 + v_i_414)], v_initial_param_953_417[(1 + v_i_414)]); 
        }
    }
}
}; 