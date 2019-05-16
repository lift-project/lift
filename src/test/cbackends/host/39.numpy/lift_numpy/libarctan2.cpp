
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
void arctan2(float * v_initial_param_143_72, float * v_initial_param_144_73, float * & v_user_func_146_76, int v_N_0){
    // Allocate memory for output pointers
    float * v_user_func_154_75 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float))));
    v_user_func_146_76 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_71 = 0;(v_i_71 <= (-1 + v_N_0)); (++v_i_71)){
        v_user_func_154_75[v_i_71] = div_uf(v_initial_param_143_72[v_i_71], v_initial_param_144_73[v_i_71]); 
    }
    // For each element processed sequentially
    for (int v_i_70 = 0;(v_i_70 <= (-1 + v_N_0)); (++v_i_70)){
        v_user_func_146_76[v_i_70] = arctan_uf(v_user_func_154_75[v_i_70]); 
    }
}
}; 