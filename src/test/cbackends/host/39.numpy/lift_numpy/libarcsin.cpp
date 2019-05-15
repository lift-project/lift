
#include <bits/stdc++.h>

using namespace std;

namespace lift {
    
#ifndef ARCSIN_UF_H
#define ARCSIN_UF_H
; 
float arcsin_uf(float x){
    { return asin(x); }; 
}

#endif
 ; 
void arcsin(float * v_initial_param_1389_237, float * & v_user_func_1391_238, int v_N_190){
    // Allocate memory for output pointers
    v_user_func_1391_238 = reinterpret_cast<float *>(malloc((v_N_190 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_236 = 0;(v_i_236 <= (-1 + v_N_190)); (++v_i_236)){
        v_user_func_1391_238[v_i_236] = arcsin_uf(v_initial_param_1389_237[v_i_236]); 
    }
}
}; 