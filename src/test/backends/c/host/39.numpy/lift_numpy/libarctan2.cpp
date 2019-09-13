
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
void arctan2(float * v_initial_param_7117_2875, float * v_initial_param_7118_2876, float * & v_user_func_7120_2879, int v_N_2763){
    // Allocate memory for output pointers
    float * v_user_func_7128_2878 = reinterpret_cast<float *>(malloc((v_N_2763 * sizeof(float))));
    v_user_func_7120_2879 = reinterpret_cast<float *>(malloc((v_N_2763 * sizeof(float)))); 
    {
        
    }
    {
        
    }
    // For each element processed sequentially
    for (int v_i_2874 = 0;(v_i_2874 <= (-1 + v_N_2763)); (++v_i_2874)){
        v_user_func_7128_2878[v_i_2874] = div_uf(v_initial_param_7117_2875[v_i_2874], v_initial_param_7118_2876[v_i_2874]); 
    }
    // For each element processed sequentially
    for (int v_i_2873 = 0;(v_i_2873 <= (-1 + v_N_2763)); (++v_i_2873)){
        v_user_func_7120_2879[v_i_2873] = arctan_uf(v_user_func_7128_2878[v_i_2873]); 
    }
}
}; 