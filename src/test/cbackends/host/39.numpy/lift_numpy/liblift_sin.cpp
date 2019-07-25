
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
void lift_sin(float * v_initial_param_3117_440, float * & v_user_func_3119_441, int v_N_352){
    // Allocate memory for output pointers
    v_user_func_3119_441 = reinterpret_cast<float *>(malloc((v_N_352 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_439 = 0;(v_i_439 <= (-1 + v_N_352)); (++v_i_439)){
        v_user_func_3119_441[v_i_439] = sin_uf(v_initial_param_3117_440[v_i_439]); 
    }
}
}; 