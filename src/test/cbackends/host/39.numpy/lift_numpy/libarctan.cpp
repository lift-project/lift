
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef ARCTAN_UF_H
#define ARCTAN_UF_H
; 
float arctan_uf(float x){
    { return atan(x); }; 
}

#endif
 ; 
void arctan(float * v_initial_param_3152_455, float * & v_user_func_3154_456, int v_N_352){
    // Allocate memory for output pointers
    v_user_func_3154_456 = reinterpret_cast<float *>(malloc((v_N_352 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_454 = 0;(v_i_454 <= (-1 + v_N_352)); (++v_i_454)){
        v_user_func_3154_456[v_i_454] = arctan_uf(v_initial_param_3152_455[v_i_454]); 
    }
}
}; 