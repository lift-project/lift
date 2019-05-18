
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef SIN_UF_H
#define SIN_UF_H
; 
float sin_uf(float x){
    { return sin(x); }; 
}

#endif
 ; 
void lift_sin(float * v_initial_param_107_69, float * & v_user_func_109_70, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_109_70 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_68 = 0;(v_i_68 <= (-1 + v_N_0)); (++v_i_68)){
        v_user_func_109_70[v_i_68] = sin_uf(v_initial_param_107_69[v_i_68]); 
    }
}
}; 