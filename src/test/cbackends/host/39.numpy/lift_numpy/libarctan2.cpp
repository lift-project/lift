
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
void arctan2(float * v_initial_param_166_99, float * v_initial_param_167_100, float * & v_user_func_169_103, int v_N_0){
    // Allocate memory for output pointers
    float * v_user_func_177_102 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float))));
    v_user_func_169_103 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_98 = 0;(v_i_98 <= (-1 + v_N_0)); (++v_i_98)){
        v_user_func_177_102[v_i_98] = div_uf(v_initial_param_166_99[v_i_98], v_initial_param_167_100[v_i_98]); 
    }
    // For each element processed sequentially
    for (int v_i_97 = 0;(v_i_97 <= (-1 + v_N_0)); (++v_i_97)){
        v_user_func_169_103[v_i_97] = arctan_uf(v_user_func_177_102[v_i_97]); 
    }
}
}; 