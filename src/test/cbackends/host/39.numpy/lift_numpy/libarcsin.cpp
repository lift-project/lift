
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
void arcsin(float * v_initial_param_131_84, float * & v_user_func_133_85, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_133_85 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_83 = 0;(v_i_83 <= (-1 + v_N_0)); (++v_i_83)){
        v_user_func_133_85[v_i_83] = arcsin_uf(v_initial_param_131_84[v_i_83]); 
    }
}
}; 