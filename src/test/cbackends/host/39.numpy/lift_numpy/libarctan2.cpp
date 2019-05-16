
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
void arctan2(float * v_initial_param_142_71, float * v_initial_param_143_72, float * & v_user_func_145_75, int v_N_0){
    // Allocate memory for output pointers
    float * v_user_func_153_74 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float))));
    v_user_func_145_75 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_70 = 0;(v_i_70 <= (-1 + v_N_0)); (++v_i_70)){
        v_user_func_153_74[v_i_70] = div_uf(v_initial_param_142_71[v_i_70], v_initial_param_143_72[v_i_70]); 
    }
    // For each element processed sequentially
    for (int v_i_69 = 0;(v_i_69 <= (-1 + v_N_0)); (++v_i_69)){
        v_user_func_145_75[v_i_69] = arctan_uf(v_user_func_153_74[v_i_69]); 
    }
}
}; 