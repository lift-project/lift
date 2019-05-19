
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
void fabs(float * v_initial_param_862_374, float * & v_user_func_864_375, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_864_375 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_373 = 0;(v_i_373 <= (-1 + v_N_0)); (++v_i_373)){
        v_user_func_864_375[v_i_373] = absolute_uf(v_initial_param_862_374[v_i_373]); 
    }
}
}; 