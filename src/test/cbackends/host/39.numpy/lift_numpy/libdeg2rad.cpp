
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef D2R_UF_H
#define D2R_UF_H
; 
float d2r_uf(float x){
    { return x*M_PI/180; }; 
}

#endif
 ; 
void deg2rad(float * v_initial_param_181_93, float * & v_user_func_183_94, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_183_94 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_92 = 0;(v_i_92 <= (-1 + v_N_0)); (++v_i_92)){
        v_user_func_183_94[v_i_92] = d2r_uf(v_initial_param_181_93[v_i_92]); 
    }
}
}; 