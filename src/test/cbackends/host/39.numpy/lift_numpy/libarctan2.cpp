
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
void arctan2(float * v_initial_param_146_75, float * v_initial_param_147_76, float * & v_user_func_149_79, int v_N_0){
    // Allocate memory for output pointers
    float * v_user_func_157_78 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float))));
    v_user_func_149_79 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_74 = 0;(v_i_74 <= (-1 + v_N_0)); (++v_i_74)){
        v_user_func_157_78[v_i_74] = div_uf(v_initial_param_146_75[v_i_74], v_initial_param_147_76[v_i_74]); 
    }
    // For each element processed sequentially
    for (int v_i_73 = 0;(v_i_73 <= (-1 + v_N_0)); (++v_i_73)){
        v_user_func_149_79[v_i_73] = arctan_uf(v_user_func_157_78[v_i_73]); 
    }
}
}; 