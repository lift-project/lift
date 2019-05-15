
#include <bits/stdc++.h>

using namespace std;

namespace lift {
    
#ifndef FIX_UF_H
#define FIX_UF_H
; 
float fix_uf(float x){
    return trunc(x) ;; 
}

#endif
 ; 
void fix(float * v_initial_param_1517_297, float * & v_user_func_1519_298, int v_N_190){
    // Allocate memory for output pointers
    v_user_func_1519_298 = reinterpret_cast<float *>(malloc((v_N_190 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_296 = 0;(v_i_296 <= (-1 + v_N_190)); (++v_i_296)){
        v_user_func_1519_298[v_i_296] = fix_uf(v_initial_param_1517_297[v_i_296]); 
    }
}
}; 