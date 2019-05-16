
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef ARCTANH_UF_H
#define ARCTANH_UF_H
; 
float arctanh_uf(float x){
    { return atanh(x); }; 
}

#endif
 ; 
void arctanh(float * v_initial_param_217_107, float * & v_user_func_219_108, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_219_108 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_106 = 0;(v_i_106 <= (-1 + v_N_0)); (++v_i_106)){
        v_user_func_219_108[v_i_106] = arctanh_uf(v_initial_param_217_107[v_i_106]); 
    }
}
}; 