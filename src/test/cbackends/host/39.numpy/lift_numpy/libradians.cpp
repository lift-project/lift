
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
void radians(float * v_initial_param_3203_473, float * & v_user_func_3205_474, int v_N_352){
    // Allocate memory for output pointers
    v_user_func_3205_474 = reinterpret_cast<float *>(malloc((v_N_352 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_472 = 0;(v_i_472 <= (-1 + v_N_352)); (++v_i_472)){
        v_user_func_3205_474[v_i_472] = d2r_uf(v_initial_param_3203_473[v_i_472]); 
    }
}
}; 