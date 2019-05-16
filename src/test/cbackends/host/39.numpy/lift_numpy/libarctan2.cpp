
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
void arctan2(float * v_initial_param_151_81, float * v_initial_param_152_82, float * & v_user_func_154_85, int v_N_0){
    // Allocate memory for output pointers
    float * v_user_func_162_84 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float))));
    v_user_func_154_85 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_80 = 0;(v_i_80 <= (-1 + v_N_0)); (++v_i_80)){
        v_user_func_162_84[v_i_80] = div_uf(v_initial_param_151_81[v_i_80], v_initial_param_152_82[v_i_80]); 
    }
    // For each element processed sequentially
    for (int v_i_79 = 0;(v_i_79 <= (-1 + v_N_0)); (++v_i_79)){
        v_user_func_154_85[v_i_79] = arctan_uf(v_user_func_162_84[v_i_79]); 
    }
}
}; 