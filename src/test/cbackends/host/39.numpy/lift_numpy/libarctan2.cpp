
#include <bits/stdc++.h>

using namespace std;

namespace lift {
    
float div_uf(float x, float y){
    { return (x)/(y); }; 
}
#ifndef ARCTAN_UF_H
#define ARCTAN_UF_H
float arctan_uf(float x){
    { return atan(x); }; 
}
#endif
void arctan2(float * v_initial_param_104_34, float * v_initial_param_105_35, float * & v_user_func_107_38, int v_N_0){
    // Allocate memory for output pointers
    float * v_user_func_115_37 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float))));
    v_user_func_107_38 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_33 = 0;(v_i_33 <= (-1 + v_N_0)); (++v_i_33)){
        v_user_func_115_37[v_i_33] = div_uf(v_initial_param_104_34[v_i_33], v_initial_param_105_35[v_i_33]); 
    }
    // For each element processed sequentially
    for (int v_i_32 = 0;(v_i_32 <= (-1 + v_N_0)); (++v_i_32)){
        v_user_func_107_38[v_i_32] = arctan_uf(v_user_func_115_37[v_i_32]); 
    }
}
}; 
