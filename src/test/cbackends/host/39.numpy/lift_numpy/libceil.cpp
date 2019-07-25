
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef CEIL_UF_H
#define CEIL_UF_H
; 
float ceil_uf(float x){
    return ceil(x);; 
}

#endif
 ; 
void ceil(float * v_initial_param_3280_515, float * & v_user_func_3282_516, int v_N_352){
    // Allocate memory for output pointers
    v_user_func_3282_516 = reinterpret_cast<float *>(malloc((v_N_352 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_514 = 0;(v_i_514 <= (-1 + v_N_352)); (++v_i_514)){
        v_user_func_3282_516[v_i_514] = ceil_uf(v_initial_param_3280_515[v_i_514]); 
    }
}
}; 