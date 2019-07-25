
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
void deg2rad(float * v_initial_param_3203_476, float * & v_user_func_3205_477, int v_N_352){
    // Allocate memory for output pointers
    v_user_func_3205_477 = reinterpret_cast<float *>(malloc((v_N_352 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_475 = 0;(v_i_475 <= (-1 + v_N_352)); (++v_i_475)){
        v_user_func_3205_477[v_i_475] = d2r_uf(v_initial_param_3203_476[v_i_475]); 
    }
}
}; 