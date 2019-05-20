
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
void arccosh(float * v_initial_param_149_142, float * & v_user_func_151_143, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_151_143 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_141 = 0;(v_i_141 <= (-1 + v_N_0)); (++v_i_141)){
        v_user_func_151_143[v_i_141] = arccos_uf(v_initial_param_149_142[v_i_141]); 
    }
}
}; 