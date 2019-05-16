
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
void arctan2(float * v_initial_param_152_83, float * v_initial_param_153_84, float * & v_user_func_155_87, int v_N_0){
    // Allocate memory for output pointers
    float * v_user_func_163_86 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float))));
    v_user_func_155_87 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_82 = 0;(v_i_82 <= (-1 + v_N_0)); (++v_i_82)){
        v_user_func_163_86[v_i_82] = div_uf(v_initial_param_152_83[v_i_82], v_initial_param_153_84[v_i_82]); 
    }
    // For each element processed sequentially
    for (int v_i_81 = 0;(v_i_81 <= (-1 + v_N_0)); (++v_i_81)){
        v_user_func_155_87[v_i_81] = arctan_uf(v_user_func_163_86[v_i_81]); 
    }
}
}; 