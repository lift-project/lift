
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
void arcsin(float * v_initial_param_116_66, float * & v_user_func_118_67, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_118_67 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_65 = 0;(v_i_65 <= (-1 + v_N_0)); (++v_i_65)){
        v_user_func_118_67[v_i_65] = arcsin_uf(v_initial_param_116_66[v_i_65]); 
    }
}
}; 