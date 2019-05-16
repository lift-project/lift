
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef HYPOT_UF_H
#define HYPOT_UF_H
; 
float hypot_uf(float x, float y){
    { return sqrt((x*x)+(y*y)); }; 
}

#endif
 ; 
void hypot(float * v_initial_param_124_61, float * v_initial_param_125_62, float * & v_user_func_131_64, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_131_64 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_60 = 0;(v_i_60 <= (-1 + v_N_0)); (++v_i_60)){
        v_user_func_131_64[v_i_60] = hypot_uf(v_initial_param_124_61[v_i_60], v_initial_param_125_62[v_i_60]); 
    }
}
}; 