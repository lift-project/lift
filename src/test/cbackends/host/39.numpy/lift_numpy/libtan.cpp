
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef TAN_UF_H
#define TAN_UF_H
; 
float tan_uf(float x){
    { return tan(x); }; 
}

#endif
 ; 
void tan(float * v_initial_param_98_51, float * & v_user_func_100_52, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_100_52 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_50 = 0;(v_i_50 <= (-1 + v_N_0)); (++v_i_50)){
        v_user_func_100_52[v_i_50] = tan_uf(v_initial_param_98_51[v_i_50]); 
    }
}
}; 