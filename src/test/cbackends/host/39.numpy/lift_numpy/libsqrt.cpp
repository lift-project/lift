
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef SQRT_UF_H
#define SQRT_UF_H
; 
float sqrt_uf(float x){
    { return sqrt(x); }; 
}

#endif
 ; 
void sqrt(float * v_initial_param_3869_732, float * & v_user_func_3871_733, int v_N_352){
    // Allocate memory for output pointers
    v_user_func_3871_733 = reinterpret_cast<float *>(malloc((v_N_352 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_731 = 0;(v_i_731 <= (-1 + v_N_352)); (++v_i_731)){
        v_user_func_3871_733[v_i_731] = sqrt_uf(v_initial_param_3869_732[v_i_731]); 
    }
}
}; 