
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef ABSOLUTE_UF_H
#define ABSOLUTE_UF_H
; 
float absolute_uf(float x){
    { return abs(x); }; 
}

#endif
 ; 
void fabs(float * v_initial_param_3890_744, float * & v_user_func_3892_745, int v_N_352){
    // Allocate memory for output pointers
    v_user_func_3892_745 = reinterpret_cast<float *>(malloc((v_N_352 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_743 = 0;(v_i_743 <= (-1 + v_N_352)); (++v_i_743)){
        v_user_func_3892_745[v_i_743] = absolute_uf(v_initial_param_3890_744[v_i_743]); 
    }
}
}; 