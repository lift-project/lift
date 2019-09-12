
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef NEGATIVE_UF_H
#define NEGATIVE_UF_H
; 
float negative_uf(float x){
    return (-1.0f)*x; 
}

#endif
 ; 
void negative(float * v_initial_param_7603_3061, float * & v_user_func_7605_3062, int v_N_2763){
    // Allocate memory for output pointers
    v_user_func_7605_3062 = reinterpret_cast<float *>(malloc((v_N_2763 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_3060 = 0;(v_i_3060 <= (-1 + v_N_2763)); (++v_i_3060)){
        v_user_func_7605_3062[v_i_3060] = negative_uf(v_initial_param_7603_3061[v_i_3060]); 
    }
}
}; 