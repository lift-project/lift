
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
void arctan2(float * v_initial_param_160_91, float * v_initial_param_161_92, float * & v_user_func_163_95, int v_N_0){
    // Allocate memory for output pointers
    float * v_user_func_171_94 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float))));
    v_user_func_163_95 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_90 = 0;(v_i_90 <= (-1 + v_N_0)); (++v_i_90)){
        v_user_func_171_94[v_i_90] = div_uf(v_initial_param_160_91[v_i_90], v_initial_param_161_92[v_i_90]); 
    }
    // For each element processed sequentially
    for (int v_i_89 = 0;(v_i_89 <= (-1 + v_N_0)); (++v_i_89)){
        v_user_func_163_95[v_i_89] = arctan_uf(v_user_func_171_94[v_i_89]); 
    }
}
}; 