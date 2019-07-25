
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef FMAX_UF_H
#define FMAX_UF_H
; 
float fmax_uf(float x, float y){
    { return max(x,y); }; 
}

#endif
 ; 
void fmax(float * v_initial_param_3932_760, float * v_initial_param_3933_761, float * & v_user_func_3939_763, int v_N_352){
    // Allocate memory for output pointers
    v_user_func_3939_763 = reinterpret_cast<float *>(malloc((v_N_352 * sizeof(float)))); 
    {
        
    }
    {
        
    }
    // For each element processed sequentially
    for (int v_i_759 = 0;(v_i_759 <= (-1 + v_N_352)); (++v_i_759)){
        v_user_func_3939_763[v_i_759] = fmax_uf(v_initial_param_3932_760[v_i_759], v_initial_param_3933_761[v_i_759]); 
    }
}
}; 