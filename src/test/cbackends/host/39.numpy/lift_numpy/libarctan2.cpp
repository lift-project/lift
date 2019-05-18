
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
void arctan2(float * v_initial_param_163_94, float * v_initial_param_164_95, float * & v_user_func_166_98, int v_N_0){
    // Allocate memory for output pointers
    float * v_user_func_174_97 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float))));
    v_user_func_166_98 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_93 = 0;(v_i_93 <= (-1 + v_N_0)); (++v_i_93)){
        v_user_func_174_97[v_i_93] = div_uf(v_initial_param_163_94[v_i_93], v_initial_param_164_95[v_i_93]); 
    }
    // For each element processed sequentially
    for (int v_i_92 = 0;(v_i_92 <= (-1 + v_N_0)); (++v_i_92)){
        v_user_func_166_98[v_i_92] = arctan_uf(v_user_func_174_97[v_i_92]); 
    }
}
}; 