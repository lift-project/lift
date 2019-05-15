
#include <bits/stdc++.h>

using namespace std;

namespace lift {
    
#ifndef ARCTANH_UF_H
#define ARCTANH_UF_H
; 
float arctanh_uf(float x){
    { return atanh(x); }; 
}

#endif
 ; 
void arctanh(float * v_initial_param_1496_285, float * & v_user_func_1498_286, int v_N_190){
    // Allocate memory for output pointers
    v_user_func_1498_286 = reinterpret_cast<float *>(malloc((v_N_190 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_284 = 0;(v_i_284 <= (-1 + v_N_190)); (++v_i_284)){
        v_user_func_1498_286[v_i_284] = arctanh_uf(v_initial_param_1496_285[v_i_284]); 
    }
}
}; 