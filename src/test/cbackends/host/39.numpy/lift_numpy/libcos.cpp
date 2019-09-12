
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef COS_UF_H
#define COS_UF_H
; 
float cos_uf(float x){
    { return cos(x); }; 
}

#endif
 ; 
void cos(float * v_initial_param_7068_2854, float * & v_user_func_7070_2855, int v_N_2763){
    // Allocate memory for output pointers
    v_user_func_7070_2855 = reinterpret_cast<float *>(malloc((v_N_2763 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_2853 = 0;(v_i_2853 <= (-1 + v_N_2763)); (++v_i_2853)){
        v_user_func_7070_2855[v_i_2853] = cos_uf(v_initial_param_7068_2854[v_i_2853]); 
    }
}
}; 