
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef ARCSIN_UF_H
#define ARCSIN_UF_H
; 
float arcsin_uf(float x){
    { return asin(x); }; 
}

#endif
 ; 
void arcsin(float * v_initial_param_106_55, float * & v_user_func_108_56, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_108_56 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_54 = 0;(v_i_54 <= (-1 + v_N_0)); (++v_i_54)){
        v_user_func_108_56[v_i_54] = arcsin_uf(v_initial_param_106_55[v_i_54]); 
    }
}
}; 