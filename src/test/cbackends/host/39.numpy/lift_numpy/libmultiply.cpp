
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef MULTIPLY_UF_H
#define MULTIPLY_UF_H
; 
float multiply_uf(float x, float y){
    return x * y;; 
}

#endif
 ; 
void multiply(float * v_initial_param_657_297, float * v_initial_param_658_298, float * & v_user_func_664_300, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_664_300 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_296 = 0;(v_i_296 <= (-1 + v_N_0)); (++v_i_296)){
        v_user_func_664_300[v_i_296] = multiply_uf(v_initial_param_657_297[v_i_296], v_initial_param_658_298[v_i_296]); 
    }
}
}; 