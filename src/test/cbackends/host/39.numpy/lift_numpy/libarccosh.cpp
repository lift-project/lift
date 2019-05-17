
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef ARCCOS_UF_H
#define ARCCOS_UF_H
; 
float arccos_uf(float x){
    { return acos(x); }; 
}

#endif
 ; 
void arccosh(float * v_initial_param_133_122, float * & v_user_func_135_123, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_135_123 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_121 = 0;(v_i_121 <= (-1 + v_N_0)); (++v_i_121)){
        v_user_func_135_123[v_i_121] = arccos_uf(v_initial_param_133_122[v_i_121]); 
    }
}
}; 