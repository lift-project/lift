
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
void arctanh(float * v_initial_param_238_132, float * & v_user_func_240_133, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_240_133 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_131 = 0;(v_i_131 <= (-1 + v_N_0)); (++v_i_131)){
        v_user_func_240_133[v_i_131] = arctanh_uf(v_initial_param_238_132[v_i_131]); 
    }
}
}; 