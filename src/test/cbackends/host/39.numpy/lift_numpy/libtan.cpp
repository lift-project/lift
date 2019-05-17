
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef TAN_UF_H
#define TAN_UF_H
; 
float tan_uf(float x){
    { return tan(x); }; 
}

#endif
 ; 
void tan(float * v_initial_param_112_67, float * & v_user_func_114_68, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_114_68 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_66 = 0;(v_i_66 <= (-1 + v_N_0)); (++v_i_66)){
        v_user_func_114_68[v_i_66] = tan_uf(v_initial_param_112_67[v_i_66]); 
    }
}
}; 