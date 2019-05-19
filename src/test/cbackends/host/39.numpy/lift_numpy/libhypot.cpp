
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
void hypot(float * v_initial_param_163_102, float * v_initial_param_164_103, float * & v_user_func_170_105, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_170_105 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_101 = 0;(v_i_101 <= (-1 + v_N_0)); (++v_i_101)){
        v_user_func_170_105[v_i_101] = hypot_uf(v_initial_param_163_102[v_i_101], v_initial_param_164_103[v_i_101]); 
    }
}
}; 