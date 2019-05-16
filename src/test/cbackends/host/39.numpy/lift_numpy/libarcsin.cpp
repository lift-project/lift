
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
void arcsin(float * v_initial_param_105_54, float * & v_user_func_107_55, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_107_55 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_53 = 0;(v_i_53 <= (-1 + v_N_0)); (++v_i_53)){
        v_user_func_107_55[v_i_53] = arcsin_uf(v_initial_param_105_54[v_i_53]); 
    }
}
}; 