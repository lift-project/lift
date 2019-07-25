
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef REMAINDER_UF_H
#define REMAINDER_UF_H
; 
float remainder_uf(float x, float y){
    if(x>=0) return x - floor(x/y)*y; else return x - round(x/y)*y; 
}

#endif
 ; 
void lift_remainder(float * v_initial_param_3757_701, float * v_initial_param_3758_702, float * & v_user_func_3764_704, int v_N_352){
    // Allocate memory for output pointers
    v_user_func_3764_704 = reinterpret_cast<float *>(malloc((v_N_352 * sizeof(float)))); 
    {
        
    }
    {
        
    }
    // For each element processed sequentially
    for (int v_i_700 = 0;(v_i_700 <= (-1 + v_N_352)); (++v_i_700)){
        v_user_func_3764_704[v_i_700] = remainder_uf(v_initial_param_3757_701[v_i_700], v_initial_param_3758_702[v_i_700]); 
    }
}
}; 