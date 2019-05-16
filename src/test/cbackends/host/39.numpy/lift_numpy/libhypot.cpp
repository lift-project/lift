
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
void hypot(float * v_initial_param_125_62, float * v_initial_param_126_63, float * & v_user_func_132_65, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_132_65 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_61 = 0;(v_i_61 <= (-1 + v_N_0)); (++v_i_61)){
        v_user_func_132_65[v_i_61] = hypot_uf(v_initial_param_125_62[v_i_61], v_initial_param_126_63[v_i_61]); 
    }
}
}; 