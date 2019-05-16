
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
void arctan2(float * v_initial_param_140_69, float * v_initial_param_141_70, float * & v_user_func_143_73, int v_N_0){
    // Allocate memory for output pointers
    float * v_user_func_151_72 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float))));
    v_user_func_143_73 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_68 = 0;(v_i_68 <= (-1 + v_N_0)); (++v_i_68)){
        v_user_func_151_72[v_i_68] = div_uf(v_initial_param_140_69[v_i_68], v_initial_param_141_70[v_i_68]); 
    }
    // For each element processed sequentially
    for (int v_i_67 = 0;(v_i_67 <= (-1 + v_N_0)); (++v_i_67)){
        v_user_func_143_73[v_i_67] = arctan_uf(v_user_func_151_72[v_i_67]); 
    }
}
}; 