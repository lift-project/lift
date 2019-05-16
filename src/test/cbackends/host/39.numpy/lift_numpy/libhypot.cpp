
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
void hypot(float * v_initial_param_122_59, float * v_initial_param_123_60, float * & v_user_func_129_62, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_129_62 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_58 = 0;(v_i_58 <= (-1 + v_N_0)); (++v_i_58)){
        v_user_func_129_62[v_i_58] = hypot_uf(v_initial_param_122_59[v_i_58], v_initial_param_123_60[v_i_58]); 
    }
}
}; 