
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef DIV_UF_H
#define DIV_UF_H
; 
float div_uf(float x, float y){
    { return (x)/(y); }; 
}

#endif
 ; 
#ifndef ARCTAN_UF_H
#define ARCTAN_UF_H
; 
float arctan_uf(float x){
    { return atan(x); }; 
}

#endif
 ; 
void arctan2(float * v_initial_param_150_79, float * v_initial_param_151_80, float * & v_user_func_153_83, int v_N_0){
    // Allocate memory for output pointers
    float * v_user_func_161_82 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float))));
    v_user_func_153_83 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_78 = 0;(v_i_78 <= (-1 + v_N_0)); (++v_i_78)){
        v_user_func_161_82[v_i_78] = div_uf(v_initial_param_150_79[v_i_78], v_initial_param_151_80[v_i_78]); 
    }
    // For each element processed sequentially
    for (int v_i_77 = 0;(v_i_77 <= (-1 + v_N_0)); (++v_i_77)){
        v_user_func_153_83[v_i_77] = arctan_uf(v_user_func_161_82[v_i_77]); 
    }
}
}; 