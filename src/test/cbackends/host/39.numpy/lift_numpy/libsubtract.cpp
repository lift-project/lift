
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef SUBTRACT_H
#define SUBTRACT_H
; 
float subtract(float l, float r){
    { return l - r; }; 
}

#endif
 ; 
void subtract(float * v_initial_param_680_296, float * v_initial_param_681_297, float * & v_user_func_687_299, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_687_299 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_295 = 0;(v_i_295 <= (-1 + v_N_0)); (++v_i_295)){
        v_user_func_687_299[v_i_295] = subtract(v_initial_param_680_296[v_i_295], v_initial_param_681_297[v_i_295]); 
    }
}
}; 