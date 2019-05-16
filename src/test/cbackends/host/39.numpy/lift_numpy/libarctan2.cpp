
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
void arctan2(float * v_initial_param_144_73, float * v_initial_param_145_74, float * & v_user_func_147_77, int v_N_0){
    // Allocate memory for output pointers
    float * v_user_func_155_76 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float))));
    v_user_func_147_77 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_72 = 0;(v_i_72 <= (-1 + v_N_0)); (++v_i_72)){
        v_user_func_155_76[v_i_72] = div_uf(v_initial_param_144_73[v_i_72], v_initial_param_145_74[v_i_72]); 
    }
    // For each element processed sequentially
    for (int v_i_71 = 0;(v_i_71 <= (-1 + v_N_0)); (++v_i_71)){
        v_user_func_147_77[v_i_71] = arctan_uf(v_user_func_155_76[v_i_71]); 
    }
}
}; 