
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef COS_UF_H
#define COS_UF_H
; 
float cos_uf(float x){
    { return cos(x); }; 
}

#endif
 ; 
void cos(float * v_initial_param_3124_443, float * & v_user_func_3126_444, int v_N_352){
    // Allocate memory for output pointers
    v_user_func_3126_444 = reinterpret_cast<float *>(malloc((v_N_352 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_442 = 0;(v_i_442 <= (-1 + v_N_352)); (++v_i_442)){
        v_user_func_3126_444[v_i_442] = cos_uf(v_initial_param_3124_443[v_i_442]); 
    }
}
}; 