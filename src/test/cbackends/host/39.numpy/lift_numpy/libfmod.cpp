
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef FMOD_UF_H
#define FMOD_UF_H
; 
float fmod_uf(float x, float y){
    return ((int)x) % ((int)y);; 
}

#endif
 ; 
void fmod(float * v_initial_param_689_297, float * v_initial_param_690_298, float * & v_user_func_696_300, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_696_300 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_296 = 0;(v_i_296 <= (-1 + v_N_0)); (++v_i_296)){
        v_user_func_696_300[v_i_296] = fmod_uf(v_initial_param_689_297[v_i_296], v_initial_param_690_298[v_i_296]); 
    }
}
}; 