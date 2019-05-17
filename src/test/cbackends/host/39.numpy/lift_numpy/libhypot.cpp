
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
void hypot(float * v_initial_param_142_81, float * v_initial_param_143_82, float * & v_user_func_149_84, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_149_84 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_80 = 0;(v_i_80 <= (-1 + v_N_0)); (++v_i_80)){
        v_user_func_149_84[v_i_80] = hypot_uf(v_initial_param_142_81[v_i_80], v_initial_param_143_82[v_i_80]); 
    }
}
}; 