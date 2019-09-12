
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef ARCCOS_UF_H
#define ARCCOS_UF_H
; 
float arccos_uf(float x){
    { return acos(x); }; 
}

#endif
 ; 
void arccos(float * v_initial_param_7089_2863, float * & v_user_func_7091_2864, int v_N_2763){
    // Allocate memory for output pointers
    v_user_func_7091_2864 = reinterpret_cast<float *>(malloc((v_N_2763 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_2862 = 0;(v_i_2862 <= (-1 + v_N_2763)); (++v_i_2862)){
        v_user_func_7091_2864[v_i_2862] = arccos_uf(v_initial_param_7089_2863[v_i_2862]); 
    }
}
}; 