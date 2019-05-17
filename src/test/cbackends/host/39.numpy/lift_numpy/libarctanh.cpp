
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
void arctanh(float * v_initial_param_230_122, float * & v_user_func_232_123, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_232_123 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_121 = 0;(v_i_121 <= (-1 + v_N_0)); (++v_i_121)){
        v_user_func_232_123[v_i_121] = arctanh_uf(v_initial_param_230_122[v_i_121]); 
    }
}
}; 