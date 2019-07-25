
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef CBRT_UF_H
#define CBRT_UF_H
; 
float cbrt_uf(float x){
    { return cbrt(x); }; 
}

#endif
 ; 
void cbrt(float * v_initial_param_3876_735, float * & v_user_func_3878_736, int v_N_352){
    // Allocate memory for output pointers
    v_user_func_3878_736 = reinterpret_cast<float *>(malloc((v_N_352 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_734 = 0;(v_i_734 <= (-1 + v_N_352)); (++v_i_734)){
        v_user_func_3878_736[v_i_734] = cbrt_uf(v_initial_param_3876_735[v_i_734]); 
    }
}
}; 