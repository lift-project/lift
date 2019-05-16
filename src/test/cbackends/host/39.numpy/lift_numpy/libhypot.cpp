
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
void hypot(float * v_initial_param_131_68, float * v_initial_param_132_69, float * & v_user_func_138_71, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_138_71 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_67 = 0;(v_i_67 <= (-1 + v_N_0)); (++v_i_67)){
        v_user_func_138_71[v_i_67] = hypot_uf(v_initial_param_131_68[v_i_67], v_initial_param_132_69[v_i_67]); 
    }
}
}; 