
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef POWER_UF_H
#define POWER_UF_H
; 
float power_uf(float x, float y){
    return pow(x, y);; 
}

#endif
 ; 
void power(float * v_initial_param_666_293, float * v_initial_param_667_294, float * & v_user_func_673_296, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_673_296 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_292 = 0;(v_i_292 <= (-1 + v_N_0)); (++v_i_292)){
        v_user_func_673_296[v_i_292] = power_uf(v_initial_param_666_293[v_i_292], v_initial_param_667_294[v_i_292]); 
    }
}
}; 