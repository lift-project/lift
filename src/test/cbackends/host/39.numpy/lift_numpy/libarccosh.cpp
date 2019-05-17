
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
void arccosh(float * v_initial_param_131_120, float * & v_user_func_133_121, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_133_121 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_119 = 0;(v_i_119 <= (-1 + v_N_0)); (++v_i_119)){
        v_user_func_133_121[v_i_119] = arccos_uf(v_initial_param_131_120[v_i_119]); 
    }
}
}; 