
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
void arctan2(float * v_initial_param_153_84, float * v_initial_param_154_85, float * & v_user_func_156_88, int v_N_0){
    // Allocate memory for output pointers
    float * v_user_func_164_87 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float))));
    v_user_func_156_88 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_83 = 0;(v_i_83 <= (-1 + v_N_0)); (++v_i_83)){
        v_user_func_164_87[v_i_83] = div_uf(v_initial_param_153_84[v_i_83], v_initial_param_154_85[v_i_83]); 
    }
    // For each element processed sequentially
    for (int v_i_82 = 0;(v_i_82 <= (-1 + v_N_0)); (++v_i_82)){
        v_user_func_156_88[v_i_82] = arctan_uf(v_user_func_164_87[v_i_82]); 
    }
}
}; 