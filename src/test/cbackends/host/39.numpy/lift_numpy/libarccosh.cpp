
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
void arccosh(float * v_initial_param_1396_282, float * & v_user_func_1398_283, int v_N_190){
    // Allocate memory for output pointers
    v_user_func_1398_283 = reinterpret_cast<float *>(malloc((v_N_190 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_281 = 0;(v_i_281 <= (-1 + v_N_190)); (++v_i_281)){
        v_user_func_1398_283[v_i_281] = arccos_uf(v_initial_param_1396_282[v_i_281]); 
    }
}
}; 