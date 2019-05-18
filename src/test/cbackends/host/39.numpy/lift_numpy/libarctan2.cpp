
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
void arctan2(float * v_initial_param_166_97, float * v_initial_param_167_98, float * & v_user_func_169_101, int v_N_0){
    // Allocate memory for output pointers
    float * v_user_func_177_100 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float))));
    v_user_func_169_101 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_96 = 0;(v_i_96 <= (-1 + v_N_0)); (++v_i_96)){
        v_user_func_177_100[v_i_96] = div_uf(v_initial_param_166_97[v_i_96], v_initial_param_167_98[v_i_96]); 
    }
    // For each element processed sequentially
    for (int v_i_95 = 0;(v_i_95 <= (-1 + v_N_0)); (++v_i_95)){
        v_user_func_169_101[v_i_95] = arctan_uf(v_user_func_177_100[v_i_95]); 
    }
}
}; 