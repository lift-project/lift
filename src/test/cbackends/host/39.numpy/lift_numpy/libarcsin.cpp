
#include <bits/stdc++.h>

using namespace std;

namespace lift {
    
#ifndef ARCSIN_UF_H
#define ARCSIN_UF_H
; 
float arcsin_uf(float x){
    { return asin(x); }; 
}

#endif; 
void arcsin(float * v_initial_param_100_46, float * & v_user_func_102_47, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_102_47 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_45 = 0;(v_i_45 <= (-1 + v_N_0)); (++v_i_45)){
        v_user_func_102_47[v_i_45] = arcsin_uf(v_initial_param_100_46[v_i_45]); 
    }
}
}; 