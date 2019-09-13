
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef TANH_UF_H
#define TANH_UF_H
; 
float tanh_uf(float x){
    { return tanh(x); }; 
}

#endif
 ; 
void tanh(float * v_initial_param_7168_2899, float * & v_user_func_7170_2900, int v_N_2763){
    // Allocate memory for output pointers
    v_user_func_7170_2900 = reinterpret_cast<float *>(malloc((v_N_2763 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_2898 = 0;(v_i_2898 <= (-1 + v_N_2763)); (++v_i_2898)){
        v_user_func_7170_2900[v_i_2898] = tanh_uf(v_initial_param_7168_2899[v_i_2898]); 
    }
}
}; 