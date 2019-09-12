
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
void arcsin(float * v_initial_param_7082_2860, float * & v_user_func_7084_2861, int v_N_2763){
    // Allocate memory for output pointers
    v_user_func_7084_2861 = reinterpret_cast<float *>(malloc((v_N_2763 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_2859 = 0;(v_i_2859 <= (-1 + v_N_2763)); (++v_i_2859)){
        v_user_func_7084_2861[v_i_2859] = arcsin_uf(v_initial_param_7082_2860[v_i_2859]); 
    }
}
}; 