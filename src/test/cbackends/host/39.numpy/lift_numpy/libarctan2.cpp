
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
void arctan2(float * v_initial_param_177_110, float * v_initial_param_178_111, float * & v_user_func_180_114, int v_N_0){
    // Allocate memory for output pointers
    float * v_user_func_188_113 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float))));
    v_user_func_180_114 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_109 = 0;(v_i_109 <= (-1 + v_N_0)); (++v_i_109)){
        v_user_func_188_113[v_i_109] = div_uf(v_initial_param_177_110[v_i_109], v_initial_param_178_111[v_i_109]); 
    }
    // For each element processed sequentially
    for (int v_i_108 = 0;(v_i_108 <= (-1 + v_N_0)); (++v_i_108)){
        v_user_func_180_114[v_i_108] = arctan_uf(v_user_func_188_113[v_i_108]); 
    }
}
}; 