
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
void fmax(float * v_initial_param_923_404, float * v_initial_param_924_405, float * & v_user_func_930_407, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_930_407 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_403 = 0;(v_i_403 <= (-1 + v_N_0)); (++v_i_403)){
        v_user_func_930_407[v_i_403] = fmax_uf(v_initial_param_923_404[v_i_403], v_initial_param_924_405[v_i_403]); 
    }
}
}; 